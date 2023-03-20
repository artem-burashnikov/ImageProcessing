module ImageProcessing.Logging

let getTime () =
    let now = System.DateTime.Now
    sprintf $"%02d{now.Hour}:%02d{now.Minute}:%02d{now.Second}:%03d{now.Millisecond}"

type LogControl =
    | Message of string
    | Stop

type Logger() =

    let agent (inbox: MailboxProcessor<LogControl>) =
        let rec loop () =
            async {
                let! msg = inbox.Receive()

                match msg with
                | Message msg ->
                    printfn $"{msg}"
                    return! loop ()
                | Stop -> () // stop by not calling a loop
            }

        loop ()

    let logger = new MailboxProcessor<LogControl>(agent)

    member _.Start() = logger.Start()
    member _.Stop() = logger.Post(Stop)
    member _.Log msg = logger.Post(Message msg)
