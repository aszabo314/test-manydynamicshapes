open System
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Rendering.Text
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open FSharp.Data.Adaptive

type Shape = {
    id : Guid
    shape : V3d[]
}

let toSg (s : Shape) (color : C4b) =
    let shape = 
        ShapeList.ofList [
            //ConcreteShape.arcPath color 1.0 (s.shape |> Array.toList |> List.map (fun p -> p.XY))
            let circle = Circle2d(s.shape.[0].XY,s.shape.[1].XY)
            yield ConcreteShape.circle color 1.0 circle
        ] |> AVal.constant
    let trafo = 
        s.shape |> Array.toList |> List.head |> (fun v -> Trafo3d.Translation(0.0,0.0,v.Z))
        |> AVal.constant
    trafo,shape

[<EntryPoint;STAThread>]
let main argv = 
    Aardvark.Init()

    let all = 10_000
    let hidden = 1_000

    let rand = RandomSystem()
    let bounds = Box3d(V3d.OOO, V3d.III*5.0)
    let randomShape() =
        //let vertices = 3 + rand.UniformInt 3
        let vertices = 2
        let shape = Array.init vertices (fun _ -> rand.UniformV3d(bounds))
        {id = Guid.NewGuid(); shape = shape}
    let generateFullRandomShapes() =
        Log.startTimed "[gen] fullrandom shape generation"
        let res = List.init all (fun _ -> randomShape())
        Log.stop()
        res |> HashSet.ofList
    let generateSameRandomShapes() =
        let s = randomShape()
        Log.startTimed "[gen] samerandom shape generation"
        let res = List.init all (fun _ -> s)
        Log.stop()
        res |> HashSet.ofList

    let allShapes = cset (generateFullRandomShapes())

    let generateHiddenGuids() =
        Log.startTimed "[gen] generate hidden guids"
        let allGuids = allShapes |> ASet.force |> Seq.toArray |> Array.map (fun s -> s.id)
        rand.Randomize allGuids
        let res = allGuids |> Array.take hidden |> List.ofArray
        Log.stop()
        res
    let generateNewColors() =
        Log.startTimed "[gen] generate colors"
        let res = allShapes |> ASet.force |> Seq.map (fun s -> s.id, rand.UniformC4d().ToC4b()) |> HashMap.ofSeq
        Log.stop()
        res
    
    let colors = AVal.init (generateNewColors())
    let hiddenGuids = AVal.init (generateHiddenGuids())

    let sg =
        allShapes
            |> ASet.chooseA (fun s -> 
                hiddenGuids |> AVal.map (fun ids -> 
                    if ids |> List.contains s.id then None
                    else Some s
                )
            )
            |> ASet.mapA (fun s -> 
                colors |> AVal.map (fun cols -> 
                     s,cols |> HashMap.tryFind s.id |> Option.defaultValue C4b.Red
                )
            )
            |> ASet.map (fun (shape,color) -> 
                toSg shape color
            )
            |> Sg.shapes

    use app = new OpenGlApplication()
    use win = app.CreateGameWindow(4)

    win.Keyboard.DownWithRepeats.Values.Add (fun k -> 
        match k with 
        | Keys.Space -> transact (fun _ -> hiddenGuids.Value <- (generateHiddenGuids()))
        | Keys.Enter -> transact (fun _ -> allShapes.Value <- (generateFullRandomShapes()))
        | Keys.RightShift -> transact (fun _ -> allShapes.Value <- (generateSameRandomShapes()))
        | Keys.C -> transact (fun _ -> colors.Value <- (generateNewColors()))
        | _ -> ()
    )













    let initialView = CameraView.lookAt (V3d(6,6,6)) V3d.Zero V3d.OOI
    let view = initialView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    let proj = win.Sizes |> AVal.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))


    let sg =
        sg
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.vertexColor |> toEffect
               ]
            |> Sg.viewTrafo (view |> AVal.map CameraView.viewTrafo)
            |> Sg.projTrafo (proj |> AVal.map Frustum.projTrafo)

    
    let task =
        app.Runtime.CompileRender(win.FramebufferSignature, sg)

    win.RenderTask <- task
    win.Run()
    0
