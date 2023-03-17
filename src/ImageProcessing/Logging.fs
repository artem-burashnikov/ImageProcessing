module ImageProcessing.Logging

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

    static member start() = logger

    static member currentWork time imgName agentName work agentId (logger: MailboxProcessor<_>) =
        let logMessage =
            $"{time} : %s{imgName} is being %s{work} by %s{agentName}#%d{agentId}"

        logger.Post(logMessage)

    static member saveStatus time imgName (logger: MailboxProcessor<_>) =
        let logMessage = $"{time} : %s{imgName} has been saved"
        logger.Post(logMessage)

    static member finishStatus time agentName agentId status (logger: MailboxProcessor<_>) =
        let logMessage = $"{time} : %s{agentName}#%d{agentId} is %s{status}"
        logger.Post(logMessage)
