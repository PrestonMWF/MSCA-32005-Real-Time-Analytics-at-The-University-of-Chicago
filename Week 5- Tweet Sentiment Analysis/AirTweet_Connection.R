## AirTweet_Connection.R
#
# Contains code providing connection and authentication to the data-streaming server.
# There is no need to change anything here!
####


# smart way to check we have specific libs installed or install them otherwise
usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1])) {
        install.packages(p, dep = TRUE);
    }
    require(p, character.only = TRUE);
}

# send protobuf message with 2 bytes of length before
SendProtobufMsg <- function(con, msg) {
    # 0. check if the message is initialized
    if ( !msg$isInitialized() ) {
        stop(paste("Protobuf message not initialized:", msg$toString()));
    }
    # 1. send message length (2 bytes) as a header
    header <- msg$bytesize();
    writeBin(header, con, size = 2, endian = "big");
    # 2. send message
    msg$serialize(con);
    return;
}


# receive raw protobuf message
ReceiveRawMsg <- function(con) {
    # 1. receive next message length as a 2-byte header
    msg_len <- readBin(con, what = "int", size = 2, signed = FALSE, endian = "big");
    # check received msg_len
    if ( length(msg_len) != 1 ) {
        stop(paste("expected to receive 1 integer as a header while received", length(msg_len)))
    }
    if (msg_len > 100000) { # normally message length shouldn't be > 100000 bytes
        stop(paste("received too large msg length: ", msg_len));
    }
    # 2. receive protobuf-message with specific length
    #return ( readBin(con, what = "raw", n = msg_len) ); ## might return <n bytes!
    msg <- raw(0);
    while (length(msg) < msg_len) {
        chunk <- readBin(con, what = "raw", n = msg_len - length(msg));
        if (length(chunk) > 0) {
            msg <- c(msg, chunk);
        }
        else { # length(chunk) <= 0
            stop("Looks like connection is lost...");
        }
    }
    return (msg);
}


# send login request and receive login reply
Authorize <- function(con, login, pwd, stream_name)
{
    # generate login message
    login_msg <- Authentication.LoginRequest$new(login = login, 
                                                 enc_password = pwd,
                                                 stream_name = stream_name);
    # send login message
    message("Sending login message");
    SendProtobufMsg(con, login_msg);
    
    # receive login-reply message and handle it
    raw_msg <- ReceiveRawMsg(con);
    login_reply <- Authentication.LoginReply$read(raw_msg);
    if (login_reply$connection_status != Authentication.LoginReply$LoginErrorsEnum$OK) {
        stop("Login failed: ", name(Authentication.LoginReply$LoginErrorsEnum$value(number = login_reply$connection_status)) );
    }
    
    # now we're logged in
    message("Logged in successfully as ", login);
}


# main function, connects to server and invokes user specified handler in the event loop
## event handler is expected to have one argument - received article (character vector of length 1) -
## and to return probability that the article is about renewable energy
Connect <- function(host, port, login, password, stream_name, event_handler, catch_handler_errors=TRUE) {
    
    problems_buf_sz <- 1000;
    current_problem_n <- 0;
    result <- list(problems=data.frame(time=.POSIXct(rep(NA, problems_buf_sz)),
                                       problem=character(problems_buf_sz),
                                       stringsAsFactors = FALSE
                                       ),
                   n_signals = 0,
                   penalty = NaN,
                   missed_id = character(0)
    );
    
    
    # connect to server
    message("Connecting to ", host, ":", port);
    con <- socketConnection(host, port, blocking = TRUE, server = FALSE, open="r+b", timeout = 120);
    # end of connection handler:
    on.exit( { close(con);  
               message("Connection closed"); 
               message("You sent total of ", result$n_signals, " signal(s) to server");
             } );
    
    # make authorization
    Authorize(con, login, password, stream_name);
    
    message("Receiving live datastream");
    
    # event-loop for server messages
    repeat {
        raw_msg <- ReceiveRawMsg(con);
        
        event_msg <- AirTweet.Event$read(raw_msg);
        # check errors
        if ( nchar(event_msg$error) > 0 ) {
            problem <- paste("SERVER SENT: '", event_msg$error, "'", sep='');
            message(problem);
            current_problem_n <- current_problem_n + 1;
            result$problems[current_problem_n,] <- list(Sys.time(), problem);
        }
        # process message from server
        if ( nchar(event_msg$tweet_id) > 0 ) { # process new tweet
            prob_v <- NULL;
            tryCatch({
                prob_v <- event_handler(event_msg$text);
                # assert prob_v length and value
                stopifnot( is.numeric(prob_v), length(prob_v) == 3, prob_v >= 0, sum(prob_v) <= 1.001, sum(prob_v) >= 0.999 );
            }, error = function(e) {
                if (catch_handler_errors) {
                    problem <- paste("Error inside handler: ", e, 'Forcing probability to (0.25, 0.5, 0.25) !', sep='');
                    message('!!!***   WARNING   ***!!!\n', 
                            problem,
                            '\n!!!*******************!!!');
                    current_problem_n <<- current_problem_n + 1;
                    result$problems[current_problem_n,] <<- list(Sys.time(), problem);
                    prob_v <<- c(0.25, 0.5, 0.25);
                }
                else {
                    stop(e);
                }
            });
            
            #message("Sending prob=", prob_v, " for tweet_id=", event_msg$tweet_id , " back to server!");
            signal_msg <- AirTweet.Signal$new(tweet_id = event_msg$tweet_id, 
                                              probability = prob_v);
            # send signal message
            SendProtobufMsg(con, signal_msg);
            result$n_signals <- result$n_signals + 1;
            #message("Successfully sent message to server!");
        }
        # check endOfStream
        if ( event_msg$stream_end ) {
            # break from repeat-loop in case server stream ends
            message("Stream has ended, goodbye!");
            result$penalty = event_msg$penalty;
            message("Your penalty=", result$penalty);
            result$missed_id <- event_msg$missed_id;
            if (length(result$missed_id) > 0) {
                message("!!! Look for missed ids in result$missed_id !!!");
            }
            result$problems <- result$problems[!is.na(result$problems$time),];
            if ( event_msg$has("score") ) {
                result$score = event_msg$score;
                message("Your score is ", result$score, " / 100");
            }
            return(result);
        }
    }
}

# POSIXct time with fractional seconds:
options(digits.secs = 6)
# parse proto-files
usePackage("RProtoBuf");
readProtoFiles(dir="./");    # read all proto-files from current folder
message("AirTweet_Connection.R sourced!");
