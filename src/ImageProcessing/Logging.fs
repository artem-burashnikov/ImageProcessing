/// <summary>
/// Provides logging functionality for image processing operations.
/// </summary>
module ImageProcessing.Logging

/// <summary>
/// Gets the current time as a formatted string.
/// </summary>
/// <returns>A formatted string representing the current time.</returns>
let getTime () =
    let now = System.DateTime.Now
    sprintf $"%02d{now.Hour}:%02d{now.Minute}:%02d{now.Second}:%03d{now.Millisecond}"

/// <summary>
/// Represents control messages for the logger agent.
/// </summary>
type LogControl =
    /// <summary>
    /// Represents a log message.
    /// </summary>
    | Message of string
    /// <summary>
    /// Represents a control message to stop the logger agent.
    /// </summary>
    | Stop

/// <summary>
/// Provides logging functionality.
/// </summary>
type Logger() =
    let mutable agentIsRunning = true

    let agent (inbox: MailboxProcessor<LogControl>) =
        async {
            while agentIsRunning do
                let! msg = inbox.Receive()

                match msg with
                | Message msg -> printfn $"{msg}"
                | Stop -> agentIsRunning <- false
        }

    let logger = new MailboxProcessor<LogControl>(agent)

    /// <summary>
    /// Starts the logger agent.
    /// </summary>
    member _.Start() = logger.Start()

    /// <summary>
    /// Stops the logger agent gracefully.
    /// </summary>
    member _.Stop() = logger.Post(Stop)

    /// <summary>
    /// Logs a message.
    /// </summary>
    /// <param name="msg">The message to log.</param>
    member _.Log(msg: string) = logger.Post(Message msg)
