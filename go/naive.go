package main

import (
	"image"
	"image/color"
	"image/png"
	_ "image/png"
	"log"
	"os"
	"sort"
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

func toGrayscale(input image.Image) *image.Gray {
	size := input.Bounds().Size()
	output := image.NewGray(input.Bounds())

	for y := input.Bounds().Min.Y; y < size.Y; y++ {
		for x := input.Bounds().Min.X; x < size.X; x++ {
			//output.SetGray(i, j, color.GrayModel.Convert(input.At(i, j)))
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

	//sliceSize := (uhb - lhb) * (uvb - lvb)
	pixels := make([]uint8, 0)
	ystride := 0

	//log.Printf("length of pixels is %d\n", len(pixels))

	for y := lvb; y < uvb; y++ {
		ystride = y * stride
		pixels = append(pixels, pix[lhb+ystride:uhb+ystride]...)
	}

	//log.Printf("length of pixels is %d\n", len(pixels))

	sort.Slice(pixels, func(i, j int) bool {
		return pixels[i] < pixels[j]
	})

	return color.Gray{pixels[len(pixels)/2]}

	/* //log.Printf("mod is %d, uhb is %d, lhb is %d, uvb is %d, lvb is %d\n", mod, uhb, lhb, uvb, lvb)

	si := img.SubImage(image.Rect(lhb, lvb, uhb, uvb)).(*image.Gray)
	//log.Printf("si: %v\n", si)
	sip := si.Pix
	sort.Slice(sip, func(i, j int) bool {
		return sip[i] < sip[j]
	})

	log.Printf("length of sip is %d\n", len(sip))

	return color.Gray{sip[len(sip)/2]} */
}

func medianFilter(img *image.Gray, windowSize int) *image.Gray {
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
	reader, err := os.Open(`D:\Users\jcoo092\Writing\2018\IVCNZ18\Images\Inputs\very small_noisy.png`)
	check(err)
	defer reader.Close()

	img, _, err := image.Decode(reader)
	check(err)

	windowSize := 3

	grayImg := toGrayscale(img)
	/* outputImage := image.NewGray(grayImg.Bounds())

	//size := outputImage.Bounds().Size()

	maxX := outputImage.Bounds().Max.X
	maxY := outputImage.Bounds().Max.Y

	for y := outputImage.Bounds().Min.Y; y < maxY; y++ {
		for x := outputImage.Bounds().Min.X; x < maxX; x++ {
			outputImage.Set(x, y, filterOnWindow(grayImg, x, y, windowSize))
		}
	} */

	outputImage := medianFilter(grayImg, windowSize)

	fg, err := os.Create(`D:\Users\jcoo092\Writing\2018\IVCNZ18\Images\Outputs\go_very_small_noisy.png`)
	defer fg.Close()
	check(err)
	err = png.Encode(fg, outputImage)
	check(err)

}
