package main

import (
	"image"
	"image/color"
	_ "image/png"
	"log"
	"os"
	"sort"
)

func toGrayscale(input image.Image) *image.Gray {
	size := input.Bounds().Size()
	output := image.NewGray(input.Bounds())

	for i := 0; i < size.X; i++ {
		for j := 0; j < size.Y; j++ {
			//output.SetGray(i, j, color.GrayModel.Convert(input.At(i, j)))
			op := input.At(i, j)
			g := color.GrayModel.Convert(op)
			output.Set(i, j, g)
		}
	}

	return output
}

func filterOnWindow(img *image.Gray, x, y, windowSize int) color.Gray {
	mod := windowSize / 2
	uhb := x + mod
	lhb := x - mod
	uvb := y + mod
	lvb := y - mod

	si := img.SubImage(image.Rect(lhb, uhb, lvb, uvb)).(*image.Gray)
	sip := si.Pix
	sort.Slice(sip, func(i, j int) bool {
		return sip[i] < sip[j]
	})

	return color.Gray{sip[len(sip)/2]}
}

func main() {
	reader, err := os.Open("testdata/video-001.q50.420.jpeg")
	if err != nil {
		log.Fatal(err)
	}
	defer reader.Close()

	img, _, err := image.Decode(reader)
	if err != nil {
		log.Fatal(err)
	}

	grayImg := toGrayscale(img)
	outputImage := image.NewGray(grayImg.Bounds())

	size := outputImage.Bounds().Size()
	windowSize := 3

	for i := 0; i < size.X; i++ {
		for j := 0; j < size.Y; j++ {
			outputImage.Set(i, j, filterOnWindow(grayImg, i, j, windowSize))
		}
	}
}
