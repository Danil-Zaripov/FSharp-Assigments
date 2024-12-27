module ImageProcessing

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

module Array2D =
    let toJagged<'a> (arr: 'a[,]) : 'a[][] =
        [| for x in 0 .. Array2D.length1 arr - 1 do
               yield [| for y in 0 .. Array2D.length2 arr - 1 -> arr.[x, y] |] |]

[<Struct>]
type Image =
    val Data: array<byte>
    val Width: int
    val Height: int
    val Name: string

    new(data, width, height, name) =
        { Data = data
          Width = width
          Height = height
          Name = name }

let loadAs2DArray (file: string) =
    let img = Image.Load<L8> file
    let res = Array2D.zeroCreate img.Height img.Width

    for i in 0 .. img.Width - 1 do
        for j in 0 .. img.Height - 1 do
            res.[j, i] <- img.Item(i, j).PackedValue

    printfn $"H=%A{img.Height} W=%A{img.Width}"
    res

let loadAsImage (file: string) =
    let img = Image.Load<L8> file

    let buf = Array.zeroCreate<byte> (img.Width * img.Height)

    img.CopyPixelDataTo(Span<byte> buf)
    Image(buf, img.Width, img.Height, System.IO.Path.GetFileName file)

let save2DByteArrayAsImage (imageData: byte[,]) file =
    let h = imageData.GetLength 0
    let w = imageData.GetLength 1
    printfn $"H=%A{h} W=%A{w}"

    let flat2Darray array2D =
        [| for x in [ 0 .. (Array2D.length1 array2D) - 1 ] do
               for y in [ 0 .. (Array2D.length2 array2D) - 1 ] do
                   yield array2D.[x, y] |]

    let img = Image.LoadPixelData<L8>(flat2Darray imageData, w, h)
    img.Save file

let saveImage (image: Image) file =
    let img = Image.LoadPixelData<L8>(image.Data, image.Width, image.Height)
    img.Save file

let gaussianBlurKernel =
    [| [| 1; 4; 6; 4; 1 |]
       [| 4; 16; 24; 16; 4 |]
       [| 6; 24; 36; 24; 6 |]
       [| 4; 16; 24; 16; 4 |]
       [| 1; 4; 6; 4; 1 |] |]
    |> Array.map (Array.map (fun x -> (float32 x) / 256.0f))

let edgesKernel =
    [| [| 0; 0; -1; 0; 0 |]
       [| 0; 0; -1; 0; 0 |]
       [| 0; 0; 2; 0; 0 |]
       [| 0; 0; 0; 0; 0 |]
       [| 0; 0; 0; 0; 0 |] |]
    |> Array.map (Array.map float32)

let idKernel =
    [| [| 0; 0; 0 |]; [| 0; 1; 0 |]; [| 0; 0; 0 |] |]
    |> Array.map (Array.map float32)

let unsharpMaskingKernel =
    [| [| 1; 4; 6; 4; 1 |]
       [| 4; 16; 24; 16; 4 |]
       [| 6; 24; -476; 24; 6 |]
       [| 4; 16; 24; 16; 4 |]
       [| 1; 4; 6; 4; 1 |] |]
    |> Array.map (Array.map (fun x -> -(float32 x) / 256.0f))

let ridgeKernel =
    [| [| 0; -1; 0 |]; [| -1; 4; -1 |]; [| 0; -1; 0 |] |]
    |> Array.map (Array.map float32)

let topSobelKernel =
    [| [| 1; 2; 1 |]; [| 0; 0; 0 |]; [| -1; -2; -1 |] |]
    |> Array.map (Array.map float32)

let sharpenKernel =
    [| [| 0; -1; 0 |]; [| -1; 5; -1 |]; [| 0; -1; 0 |] |]
    |> Array.map (Array.map float32)

let outlineKernel =
    [| [| -1; -1; -1 |]; [| -1; 8; -1 |]; [| -1; -1; -1 |] |]
    |> Array.map (Array.map float32)

let shiftRightKernel =
    [| [| 0; 0; 0 |]; [| 1; 0; 0 |]; [| 0; 0; 0 |] |]
    |> Array.map (Array.map float32)

let shiftDownKernel =
    [| [| 0; 1; 0 |]; [| 0; 0; 0 |]; [| 0; 0; 0 |] |]
    |> Array.map (Array.map float32)

let shiftRightDownKenrel =
    [| [| 1; 0; 0 |]; [| 0; 0; 0 |]; [| 0; 0; 0 |] |]
    |> Array.map (Array.map float32)

let blackKernel = [| [| 0 |] |] |> Array.map (Array.map float32)

let whiteKernel = [| [| 255 |] |] |> Array.map (Array.map float32)

let expand times kernel =
    let n, m = Array.length kernel, Array.length kernel[0]
    let expanded = Array2D.zeroCreate (n + 2 * times) (m + 2 * times)

    for i in times .. n - 1 + times do
        for j in times .. m - 1 + times do
            expanded[i, j] <- kernel[i - times][j - times]

    Array2D.toJagged expanded

let applyFilter (filter: float32[][]) (img: byte[,]) =
    let imgH = img.GetLength 0
    let imgW = img.GetLength 1

    let filterD = (Array.length filter) / 2

    let filter = Array.concat filter

    let processPixel px py =
        let dataToHandle =
            [| for i in px - filterD .. px + filterD do
                   for j in py - filterD .. py + filterD do
                       if i < 0 || i >= imgH || j < 0 || j >= imgW then
                           float32 img.[px, py]
                       else
                           float32 img.[i, j] |]

        Array.fold2 (fun s x y -> s + x * y) 0.0f filter dataToHandle

    let ceiledProcess x y _ =
        let processed = processPixel x y |> int
        let ceiled = min 255 (max 0 processed)
        byte ceiled

    Array2D.mapi ceiledProcess img
