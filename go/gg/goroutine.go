package main

import (
	"image"
	"image/color"
	"image/png"
	"log"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"sync"
)

// All of the below are set once, and then not altered during a run of the median filter
var stride int
var intensities []uint8
var channels []chan uint8
var totalPixels int
var windowSize int
var offset int
var minX int
var minY int
var maxX int
var maxY int
var waitGroup sync.WaitGroup
var outputSlice []uint8

func min(x, y int) int {
	if x < y {
		return x
	}
	return y
}

func max(x, y int) int {
	if x > y {
		return x
	}
	return y
}

func check(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

// ToGrayscale takes an arbitrary-color image, and converts it to 8-bit grayscale
func ToGrayscale(input image.Image) *image.Gray {
	size := input.Bounds().Size()
	output := image.NewGray(input.Bounds())

	for y := input.Bounds().Min.Y; y < size.Y; y++ {
		for x := input.Bounds().Min.X; x < size.X; x++ {
			op := input.At(x, y)
			g := color.GrayModel.Convert(op)
			output.Set(x, y, g)
		}
	}

	return output
}

func runPixel(intensity uint8, position int, outputSlice []uint8) {
	// do stuff
	selfX := position / stride
	selfY := position % stride

	lhb := max(minX, selfX-offset)
	uhb := min(maxX, selfX+offset)
	lvb := max(minY, selfY-offset)
	uvb := min(maxY, selfY+offset)

	window := make([]uint8, windowSize*windowSize)
	window[0] = intensity
	idx := 1

	//neighboursList := make([]int, (uhb-lhb)*(uvb-lvb)-1)
	//neighbourListIndex := 0

	for y := lvb; y < uvb; y++ {
		for x := lhb; x < uhb; x++ {
			if y == 0 && x == 0 {
				continue
			} else {
				select {
				case channels[position] <- intensity:

				case val := <-channels[x+y*stride]:
					window[idx] = val
					idx++
				}
			}
		}
	}

	sort.Slice(window, func(i, j int) bool {
		return window[i] < window[j]
	})

	// send value back to somewhere...
	outputSlice[position] = window[len(window)/2]
	waitGroup.Done()

	for {
		channels[position] <- intensity
	}
}

// MedianFilter takes a grayscale image and a window size, then performs a basic median filtering on the image with the given window size
func MedianFilter(img *image.Gray, windowSize int) *image.Gray {
	outputImage := image.NewGray(img.Bounds())

	minX = outputImage.Bounds().Min.X
	minY = outputImage.Bounds().Min.Y
	maxX = outputImage.Bounds().Max.X
	maxY = outputImage.Bounds().Max.Y

	// for y := outputImage.Bounds().Min.Y; y < maxY; y++ {
	// 	for x := outputImage.Bounds().Min.X; x < maxX; x++ {
	// 		outputImage.Set(x, y, filterOnWindow(img, x, y, windowSize))
	// 	}
	// }
	// return outputImage
	intensities = img.Pix
	stride = img.Stride
	totalPixels = len(intensities)
	channels = make([]chan uint8, totalPixels)
	outputSlice = make([]uint8, totalPixels)

	waitGroup.Add(totalPixels)

	for i, pix := range intensities {
		channels[i] = make(chan uint8)
		go runPixel(pix, i, outputSlice)
	}

	// for y := outputImage.Bounds().Min.Y; y < maxY; y++ {
	// 	for x := outputImage.Bounds().Min.X; x < maxX; x++ {
	// 		outputImage.Set(x, y, filterOnWindow(img, x, y, windowSize))
	// 	}
	// }

	waitGroup.Wait()

	outputImage.Pix = outputSlice
	return outputImage
}

func main() {
	commandLineArgs := os.Args[1:]
	wS, err := strconv.Atoi(commandLineArgs[0])
	check(err)
	windowSize = wS
	offset = (windowSize - 1) / 2

	reader, err := os.Open(commandLineArgs[1])
	defer reader.Close()
	check(err)

	img, _, err := image.Decode(reader)
	check(err)

	grayImg := ToGrayscale(img)
	outputImage := MedianFilter(grayImg, windowSize)

	outputFilename := filepath.Base(commandLineArgs[1])
	outputFilename = "goroutine_" + outputFilename

	fg, err := os.Create(`D:\Users\jcoo092\Writing\2018\IVCNZ18\Images\Outputs\` + outputFilename)
	defer fg.Close()
	check(err)

	err = png.Encode(fg, outputImage)
	check(err)

}
