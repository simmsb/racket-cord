#lang scribble/manual
@require[@for-label[racket-cord
                    json
                    net/rfc6455]
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
  [voice-states jsexpr?]
  [members (listof member?)]
  [channels (hash/c string? guild-channel?)]
  [presences (listof jsexpr?)])]

@defstruct*[
  guild-channel
  ([id string?]
  [type integer?]
  [guild-id string?]
  [position integer?]
  [permission-overwrites (listof jsexpr?)]
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
  [attachments jsexpr?]
  [embeds jsexpr?]
  [reactions jsexpr?]
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

Each event has a raw counterpart, for example @racket['raw-message-create].
All raw events have the form:

@defproc[(raw-event [ws-client ws-client?]
                    [client client?]
                    [data jsexpr?]) void?]

@section{Miscellaneous functions}

@defproc[(get-channels [client client?]) (listof channel?)]

@defproc[(get-channel [client client?]
                      [id string?]) (or/c channel? null?)]{
Get a channel by id. Returns @racket[null] on failure.
}

@defproc[(get-guild [client client?]
                    [id string?]) (or/c guild? null?)]{
Get a guild by id. Returns @racket[null] on failure.
}

@defproc[(get-member [client client?]
                     [member-id string?]
                     [guild-id string?]) (or/c member? null?)]{
Get a member of a guild by id. Returns null on failure.
}

@section{HTTP}

HTTP requests are defined here. Ratelimiting is handled for you by the library.
Requests that fail raise a @racket[exn:fail:network:http:discord?] exception.

@defproc[(http:get-channel [client client?]
                      [channel-id string?])
                      channel?]{
Request a channel.
}

@defproc[(http:modify-channel [client client?]
                              [channel-id string?]
                              [data hash?])
                              channel?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/channel#modify-channel"]{Modify Channel}.
}

@defproc[(http:delete-channel [client client?]
                              [channel-id string?]) jsexpr?]

@defproc[(http:get-channel-messages [client client?]
                                    [channel-id string?]
                                    [params (cons/c string? string?)] ...)
                                    (listof message?)]{
Params provided should be cons cells of @racket['(k . v)] conforming to @link["https://discordapp.com/developers/docs/resources/channel#get-channel-messages"]{Get Channel Message}.
}

@defproc[(http:get-channel-message [client client?]
                                   [channel-id string?]
                                   [message-id string?]) message?]

@defproc[(http:create-message [client client?]
                              [content string?]
                              [#:embed embed jsexpr? null]
                              [#:tts tts boolean? #f]) message?]

@defproc[(http:edit-message [client client?]
                            [channel-id string?]
                            [message-id string?]
                            [#:content content (or/c string? null?) null]
                            [#:embed embed jsexpr? null]) message?]

@defproc[(http:delete-message [client client?]
                              [channel-id string?]
                              [message-id string?]) jsexpr?]

@defproc[(http:create-reaction [client client?]
                               [channel-id string?]
                               [message-id string?]
                               [emoji string?]) jsexpr?]

@defproc[(http:delete-own-reaction [client client?]
                                   [channel-id string?]
                                   [message-id string?]
                                   [emoji string?]) jsexpr?]

@defproc[(http:delete-user-reaction [client client?]
                                    [channel-id string?]
                                    [message-id string?]
                                    [emoji string?]
                                    [user-id string?]) jsexpr?]

@defproc[(http:get-reactions [client client?]
                             [channel-id string?]
                             [message-id string?]
                             [emoji string?]
                             [params (cons string? string?)] ...)
                             (listof user?)]{
Params provided should be cons cells of @racket['(k . v)] conforming to @link["https://discordapp.com/developers/docs/resources/channel#get-reactions"]{Get Reactions}.
}

@defproc[(http:delete-all-reactions [client client?]
                                    [channel-id string?]
                                    [message-id string?]) jsexpr?]

@defproc[(http:bulk-delete-messages [client client?]
                                    [channel-id string?]
                                    [ids string?] ...) jsexpr?]

@defproc[(http:edit-channel-permissions [client client?]
                                        [channel-id string?]
                                        [overwrite-id string?]
                                        [allow integer?]
                                        [deny integer?]
                                        [type string?]) jsexpr?]

@defproc[(http:get-channel-invites [client client?]
                                   [channel-id string?]) jsexpr?]

@defproc[(http:create-channel-invite [client client?]
                                     [channel-id string?]
                                     [age integer? 86400]
                                     [uses integer? 0]
                                     [temporary boolean? #f]
                                     [unique boolean #f]) invite?]

@defproc[(http:delete-channel-permission [client client?]
                                         [channel-id string?]
                                         [overwrite-id string?]) jsexpr?]

@defproc[(http:trigger-typing-indicator [client client?]
                                        [channel-id string?]) jsexpr?]

@defproc[(http:get-pinned-messages [client client?]
                                   [channel-id string?]) (listof message?)]

@defproc[(http:add-pinned-channel-message [client client?]
                                          [channel-id string?]
                                          [message-id string?]) jsexpr?]

@defproc[(http:delete-pinned-channel-message [client client?]
                                             [channel-id string?]
                                             [message-id string?]) jsexpr?]

@defproc[(http:group-dm-add-recipient [client client?]
                                      [channel-id string?]
                                      [user-id string?]
                                      [access-token string?]
                                      [nick string?]) jsexpr?]

@defproc[(http:group-dm-remove-recipient [client client?]
                                         [channel-id string?]
                                         [user-id string?]) jsexpr?]

@defproc[(http:list-guild-emoji [client client?]
                                [guild-id string?]) (listof emoji?)]

@defproc[(http:get-guild-emoji [client client?]
                               [guild-id string?]
                               [emoji-id string?]) emoji?]

@defproc[(http:create-guild-emoji [client client?]
                                  [guild-id string?]
                                  [name string?]
                                  [image bytes?]
                                  [image-type string?]
                                  [roles (listof string?)]) emoji?]

@defproc[(http:modify-guild-emoji [client client?]
                                  [guild-id string?]
                                  [emoji-id string?]
                                  [name string?]
                                  [roles (listof string?)]) emoji?]

@defproc[(http:delete-guild-emoji [client client?]
                                  [guild-id string?]
                                  [emoji-id string?]) jsexpr?]

@section{Exceptions}

@defstruct*[
  http:exn:fail:network:http:discord
  ([message string?]
  [continuation-marks continuation-mark-set?]
  [code number?]
  [json-code number?]
  [message string?])
  #:transparent]{
  Raised when a http error code is retrieved from discord.

code: The HTTP response code.

json-code: The error code sent by discord.

message: The error string sent by discord.
}
