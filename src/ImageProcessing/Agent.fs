module ImageProcessing.Agent

open ImageProcessing.ImageProcessing
open ImageProcessing.Logging

type Message =
    | Img of Image
    | EOS of AsyncReplyChannel<unit>

let outFile outDir (imgName: string) = System.IO.Path.Combine(outDir, imgName)

type ImageAgent() =
    static let processorAndSaver id (transformation: Image -> Image) outDir =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            Logger.log $"%s{(getTime ())} : Agent#%d{id} is finished"
                            ch.Reply()
                        | Img img ->
                            Logger.log $"%s{(getTime ())} : %s{img.Name} is being processed by Agent#%d{id}"
                            let transformedImg = transformation img
                            Logger.log $"%s{(getTime ())} : %s{img.Name} is being saved by Agent#%d{id}"
                            saveImage transformedImg (outFile outDir img.Name)
                            Logger.log $"%s{(getTime ())} : %s{img.Name} has been saved successfully by Agent#%d{id}"
                            return! loop ()
                    }

                loop ())

    static let processor id (transformation: Image -> Image) (imgSaver: MailboxProcessor<_>) =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            Logger.log $"%s{(getTime ())} : ProcessorAgent#%d{id} is ready to finish"
                            imgSaver.PostAndReply(EOS)
                            Logger.log $"%s{(getTime ())} : ProcessorAgent#%d{id} is finished"
                            ch.Reply()
                        | Img img ->
                            Logger.log $"%s{(getTime ())} : %s{img.Name} is being processed by ProcessorAgent#%d{id}"
                            let transformedImg = transformation img
                            imgSaver.Post(Img transformedImg)
                            return! loop ()
                    }

                loop ())

    static let saver id outDir =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            Logger.log $"%s{(getTime ())} : SavingAgent#%d{id} is ready to finish"
                            ch.Reply()
                        | Img img ->
                            Logger.log $"%s{(getTime ())} : %s{img.Name} is being saved by SavingAgent#%d{id}"
                            saveImage img (outFile outDir img.Name)

                            Logger.log
                                $"%s{(getTime ())} : %s{img.Name} has been saved successfully by SavingAgent#%d{id}"

                            return! loop ()
                    }

                loop ())

    static member startProcessorAndSaver id transformation outDir =
        processorAndSaver id transformation outDir

    static member startProcessor id transformation imgSaver = processor id transformation imgSaver

    static member startSaver id outDir = saver id outDir
