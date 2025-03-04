namespace Tests

open Xunit
open ImageProcessing


module UnitTests =
    let imagesFolder = "../../../../../Images/"
    let samplePath = imagesFolder + "Koshak.png"

    let checkFilter filename filter =
        let path = imagesFolder + filename
        let expected = loadAs2DArray path

        let actual = loadAs2DArray samplePath |> applyFilter filter
        Assert.Equal(expected, actual)

    [<Fact>]
    let checkGaussianBlur () =
        checkFilter "Koshak-gaussblur.png" gaussianBlurKernel

    [<Fact>]
    let checkEdges () =
        checkFilter "Koshak-edges.png" edgesKernel

    [<Fact>]
    let checkUnsharpMasking () =
        checkFilter "Koshak-unsharpMasking.png" unsharpMaskingKernel

    [<Fact>]
    let checkRidge () =
        checkFilter "Koshak-ridge.png" ridgeKernel

    [<Fact>]
    let checkTopSobel () =
        checkFilter "Koshak-topSobel.png" topSobelKernel

    [<Fact>]
    let checkOutline () =
        checkFilter "Koshak-outline.png" outlineKernel

    [<Fact>]
    let checkSharpen () =
        checkFilter "Koshak-sharpen.png" sharpenKernel
