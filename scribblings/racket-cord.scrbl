#lang scribble/manual
@require[@for-label[racket-cord]
                   @for-label[@except-in[racket/base member]]]

@title{racket-cord: Racket discord library}
@author{@author+email["Ben Simms" "ben@bensimms.moe"]}

@defmodule[racket-cord]

@section{Example}

Example usage of the library:

@racketblock[
(require racket-cord)

(define bot-token (getenv "BOT_TOKEN"))

(define myclient (make-client bot-token #:auto-shard #t))

(on-event
  'message-create myclient
  (lambda (client message)
    (unless (string=? (user-id (message-author message)) ; dont reply to ourselves
                      (user-id (client-user client)))
      (cond
        [(string-prefix? (message-content message) "!echo ")
         (http:create-message client (message-channel-id message)
                              (string-trim (message-content message) "!echo " #:right? #f))]))))

;; logging stufff

(define dr (make-log-receiver discord-logger 'debug))

(thread
  (thunk
    (let loop ()
      (let ([v (sync dr)])
        (printf "[~a] ~a\n" (vector-ref v 0)
                (vector-ref v 1)))
      (loop))))

(start-client myclient)]

@section{Client}

@defproc[(make-client [token string?]
                      [#:token-type token-type symbol? 'bot]
                      [#:auto-shard auto-shard boolean? #f]
                      [#:shard-count shard-count integer? 1]) client?]{
Makes a discord client with the passed token.

@racket[token]: The token to use.

@racket[#:token-type]: The type of token, either @racket['bot], @racket['bearer] or @racket['client].

@racket[#:auto-shard]: Whether to request the number of shards from discord, only usable for bot clients.

@racket[#:shard-count]: If auto-shard is not passed, the number of shards to connect will be such.}


@defproc[(start-client [client client?]) void?]{
Start a client. WS is connected and the client begins handling events.

This function blocks until the client is disconnected through @racket[stop-client].
}

@defproc[(strart-client-no-wait [client client?]) void?]{
Same as @racket[start-client] but does not block the calling thread.
}

@defproc[(stop-client [client client?]) void?]{
Stops a client.
}

@defproc[(update-status [client client?]
                        [guild-id integer?]
                        [#:since since integer? null]
                        [#:game game game? null]
                        [#:status status string? "online"]
                        [#:afk afk boolean? #f]) void?]{
Updates the status of a client.
}

@defproc[(on-event [evt symbol?]
                   [client client?]
                   [callback procedure?]) void?]{
Register an event on the client.
The type of procedure passed is described in @secref{events}.

@racket[evt]: A symbol of the event name, for example @racket['message-create]
}

@section{Data}

@defstruct*[
  client
  ([shards list?]
  [user user?]
  [guilds (hash/c string? guild?)]
  [private-channels (hash/c string? dm-channel?)]
  [events (hash/c symbol? procedure?)]
  [http-client http:http-client?]
  [token string?]
  [running semaphore?])
  #:mutable
  #:transparent]{
Stores the state of the client.
}

@defstruct*[
  guild
  ([shard-id integer?]
  [id string?]
  [name string?]
  [icon string?]
  [splash string?]
  [owner-id string?]
  [region string?]
  [afk-channel-id string?]
  [afk-timeout integer?]
  [embed-enabled boolean?]
  [embed-channel-id string?]
  [verification-level integer?]
  [default-message-notifications integer?]
  [explicit-content-filter integer?]
  [roles (hash/c string? role?)]
  [emojis (hash/c string? emoji?)]
  [features (listof string?)]
  [mfa-level integer?]
  [application-id string?]
  [widget-enabled boolean?]
  [widget-channel-id string?]
  [joined-at string?]
  [large boolean?]
  [member-count integer?]
  [voice-states any/c]
  [members (listof member?)]
  [channels (hash/c string? guild-channel?)]
  [presences (listof any/c)])]

@defstruct*[
  guild-channel
  ([id string?]
  [type integer?]
  [guild-id string?]
  [position integer?]
  [permission-overwrites (listof any/c)]
  [name string?]
  [topic string?]
  [nsfw boolean?]
  [last-message-id string?]
  [bitrate integer?]
  [user-limit integer?]
  [parent-id string?])]{
  An object representing a guild text channel or a guild voice channel.
  Fields that only exist for voice channels will be @racket[null] for a text channel, and vice-versa.
}

@defstruct*[
  dm-channel
  ([id string?]
  [type integer?]
  [name string?]
  [last-message-id string?]
  [icon string?]
  [recipients string?]
  [owner-id string?]
  [application-id string?])]

@defstruct*[
  user
  ([id string?]
  [username string?]
  [discriminator string?]
  [avatar string?]
  [bot boolean?]
  [mfa-enabled boolean?])]

@defstruct*[
  member
  ([user user?]
  [nick (or/c string? null?)]
  [roles (listof string?)]
  [joined-at string?]
  [deaf boolean?]
  [mute boolean?]
  [status (or/c string? null?)]
  [game (or/c game? null?)])]

@defstruct*[
  message
  ([id string?]
  [channel-id string?]
  [author (or/c user? null?)]
  [content string?]
  [timestamp string?]
  [edited-timestamp (or/c string? null?)]
  [tts boolean?]
  [mention-everyone boolean?]
  [mentions (listof user?)]
  [mention-roles (listof role?)]
  [attachments any/c]
  [embeds any/c]
  [reactions any/c]
  [pinned boolean?]
  [type integer?])]

@defstruct*[
  role
  ([id string?]
  [name string?]
  [color integer?]
  [hoist boolean?]
  [position integer?]
  [permissions integer?]
  [managed boolean?]
  [mentionable boolean?])]

@defstruct*[
  emoji
  ([id string?]
  [name string?]
  [roles (listof string?)]
  [user (or/c user? null?)]
  [require-colons boolean?]
  [managed boolean?])]

@defstruct*[
  game
  ([name string?]
  [type integer?]
  [url string?])]

@defstruct*[
  invite
  ([code string?]
  [guild-id string?]
  [channel-id string?])]

@defstruct*[
  webhook
  ([id string?]
  [guild-id (or/c string? null?)]
  [channel-id string?]
  [user (or/c user?)]
  [name (or/c string? null?)]
  [avatar (or/c string? null?)]
  [token string?])]

@section[#:tag "events"]{Events}

User the procedure @racket[on-event] to register an event.

Example:
@racketblock[
  (on-event 'message-create
   (lambda (client message)
     (println (message-content message))))
]

Event callbacks and their types are described here:

@deftogether[(
  @defproc[(channel-create [client client?]
                           [channel channel?]) void?]
  @defproc[(channel-delete [client client?]
                           [channel channel?]) void?]
  @defproc[(channel-update [client client?]
                           [old-channel channel?]
                           [new-channel channel?]) void?]
  @defproc[(guild-create [client client?]
                         [guild guild?]) void?]
  @defproc[(guild-delete [client client?]
                         [guild guild?]) void?]
  @defproc[(guild-update [client client?]
                         [old-guild guild?]
                         [new-guild guild?]) void?]
  @defproc[(guild-ban-add [client client?]
                          [user user?]
                          [guild guild?]) void?]
  @defproc[(guild-ban-remove [client client?]
                             [user user?]
                             [guild guild?]) void?]
  @defproc[(guild-emojis-update [client client?]
                                [guild guild?]
                                [emojis (listof emoji?)]) void?]
  @defproc[(guild-member-add [client client?]
                             [member member?]) void?]
  @defproc[(guild-member-remove [client client?]
                                [member member?]) void?]
  @defproc[(guild-member-update [client client?]
                                [old-member member?]
                                [new-member member?]) void?]
  @defproc[(presence-update [client client?]
                            [old-member member?]
                            [new-member member?]) void?]
  @defproc[(message-create [client client?]
                           [message message?]) void?]
  @defproc[(message-delete [client client?]
                           [message-id string?]) void?]
  @defproc[(message-reaction-add [client client?]
                                 [user-id string?]
                                 [channel-id string?]
                                 [message-id string?]
                                 [emoji emoji?]) void?]
  @defproc[(message-reaction-remove [client client?]
                                    [user-id string?]
                                    [channel-id string?]
                                    [message-id string?]
                                    [emoji emoji?]) void?]
  @defproc[(message-reaction-remove-all [client client?]
                                        [channel-id string?]
                                        [message-id string?]) void?]
  @defproc[(typing-start [client client?]
                         [channel-id string?]
                         [user-id string?]) void?])]
