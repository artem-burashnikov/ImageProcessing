module ImageProcessing.Logging

let getTime () =
    let now = System.DateTime.Now
    sprintf $"%02d{now.Hour}:%02d{now.Minute}:%02d{now.Second}:%03d{now.Millisecond}"

type Logger() =

    static let logger =
        MailboxProcessor<string>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()
                        printfn $"{msg}"
                        return! loop ()
                    }

                loop ())

    static member log msg = logger.Post(msg)
