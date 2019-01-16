package naive

import (
	"image"
	"os"
	"testing"
)

func benchmarkNaive(filename string, windowSize int, b *testing.B) {
	filepath := `D:\Users\jcoo092\Writing\2018\IVCNZ18\Images\Inputs\`
	reader, err := os.Open(filepath + filename)
	defer reader.Close()
	check(err)

	img, _, err := image.Decode(reader)
	check(err)

	grayImg := ToGrayscale(img)

	for n := 0; n < b.N; n++ {
		MedianFilter(grayImg, windowSize)
	}
}

func BenchmarkNaiveVerySmall3(b *testing.B) {
	benchmarkNaive("very small_noisy.png", 3, b)
}

func BenchmarkNaiveVerySmall5(b *testing.B) {
	benchmarkNaive("very small_noisy.png", 5, b)
}

func BenchmarkNaiveVerySmall7(b *testing.B) {
	benchmarkNaive("very small_noisy.png", 7, b)
}

func BenchmarkNaiveVerySmall9(b *testing.B) {
	benchmarkNaive("very small_noisy.png", 9, b)
}

func BenchmarkNaiveVerySmall11(b *testing.B) {
	benchmarkNaive("very small_noisy.png", 11, b)
}

func BenchmarkNaiveSmall3(b *testing.B) {
	benchmarkNaive("small_noisy.png", 3, b)
}

func BenchmarkNaiveSmall5(b *testing.B) {
	benchmarkNaive("small_noisy.png", 5, b)
}

func BenchmarkNaiveSmall7(b *testing.B) {
	benchmarkNaive("small_noisy.png", 7, b)
}

func BenchmarkNaiveSmall9(b *testing.B) {
	benchmarkNaive("small_noisy.png", 9, b)
}

func BenchmarkNaiveSmall11(b *testing.B) {
	benchmarkNaive("small_noisy.png", 11, b)
}

func BenchmarkNaivePeppers3(b *testing.B) {
	benchmarkNaive("peppers_gray_noisy.png", 3, b)
}

func BenchmarkNaivePeppers5(b *testing.B) {
	benchmarkNaive("peppers_gray_noisy.png", 5, b)
}

func BenchmarkNaivePeppers7(b *testing.B) {
	benchmarkNaive("peppers_gray_noisy.png", 7, b)
}

func BenchmarkNaivePeppers9(b *testing.B) {
	benchmarkNaive("peppers_gray_noisy.png", 9, b)
}

func BenchmarkNaivePeppers11(b *testing.B) {
	benchmarkNaive("peppers_gray_noisy.png", 11, b)
}

func BenchmarkNaiveMedium3(b *testing.B) {
	benchmarkNaive("medium_noisy.png", 3, b)
}

func BenchmarkNaiveMedium5(b *testing.B) {
	benchmarkNaive("medium_noisy.png", 5, b)
}

func BenchmarkNaiveMedium7(b *testing.B) {
	benchmarkNaive("medium_noisy.png", 7, b)
}

func BenchmarkNaiveMedium9(b *testing.B) {
	benchmarkNaive("medium_noisy.png", 9, b)
}

func BenchmarkNaiveMedium11(b *testing.B) {
	benchmarkNaive("medium_noisy.png", 11, b)
}

/* type testCase struct {
	filename   string
	windowSize int
}

func BenchmarkNaive(b *testing.B) {
	windowSizes := [5]int{3, 5, 7, 9, 11}
	filenames := [4]string{"very small_noisy.png", "small_noisy.png", "peppers_gray_noisy.png", "medium_noisy.png"}

	testCases := make([]testCase, 5*4)

	for i, fn := range filenames {
		for j, ws := range windowSizes {
			log.Printf("i = %d, j = %d, j+i*4 = %d", i, j, j+i*5)
			testCases[j+i*5] = testCase{fn, ws}
		}
	}

	for _, tc := range testCases {
		filename := `D:\Users\jcoo092\Writing\2018\IVCNZ18\Images\Inputs\` + tc.filename
		reader, err := os.Open(filename)
		defer reader.Close()
		check(err)

		img, _, err := image.Decode(reader)
		check(err)

		grayImg := ToGrayscale(img)

		for n := 0; n < b.N; n++ {
			MedianFilter(grayImg, tc.windowSize)
		}
	}

} */
