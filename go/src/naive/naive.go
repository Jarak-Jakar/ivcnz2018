package naive

import (
	"image"
	"image/color"
	"image/png"
	"log"
	"os"
	"path/filepath"
	"sort"
	"strconv"
)

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

func filterOnWindow(img *image.Gray, x, y, windowSize int) color.Gray {
	mod := windowSize / 2
	uhb := min(img.Bounds().Max.X, x+mod+1)
	lhb := max(img.Bounds().Min.X, x-mod)
	uvb := min(img.Bounds().Max.Y, y+mod+1)
	lvb := max(img.Bounds().Min.Y, y-mod)
	stride := img.Stride
	pix := img.Pix

	pixels := make([]uint8, 0)
	ystride := 0

	for y := lvb; y < uvb; y++ {
		ystride = y * stride
		pixels = append(pixels, pix[lhb+ystride:uhb+ystride]...)
	}

	sort.Slice(pixels, func(i, j int) bool {
		return pixels[i] < pixels[j]
	})

	return color.Gray{pixels[len(pixels)/2]}
}

// MedianFilter takes a grayscale image and a window size, then performs a basic median filtering on the image with the given window size
func MedianFilter(img *image.Gray, windowSize int) *image.Gray {
	outputImage := image.NewGray(img.Bounds())

	maxX := outputImage.Bounds().Max.X
	maxY := outputImage.Bounds().Max.Y

	for y := outputImage.Bounds().Min.Y; y < maxY; y++ {
		for x := outputImage.Bounds().Min.X; x < maxX; x++ {
			outputImage.Set(x, y, filterOnWindow(img, x, y, windowSize))
		}
	}
	return outputImage
}

func main() {
	commandLineArgs := os.Args[1:]
	windowSize, err := strconv.Atoi(commandLineArgs[0])
	check(err)

	reader, err := os.Open(commandLineArgs[1])
	defer reader.Close()
	check(err)

	img, _, err := image.Decode(reader)
	check(err)

	grayImg := ToGrayscale(img)
	outputImage := MedianFilter(grayImg, windowSize)

	outputFilename := filepath.Base(commandLineArgs[1])
	outputFilename = "go_" + outputFilename

	fg, err := os.Create(`D:\Users\jcoo092\Writing\2018\IVCNZ18\Images\Outputs\` + outputFilename)
	defer fg.Close()
	check(err)

	err = png.Encode(fg, outputImage)
	check(err)

}
