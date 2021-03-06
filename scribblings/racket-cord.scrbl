#lang scribble/manual
@require[scribble/extract]
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

@include-previously-extracted["extracted.scrbl" "make-client"]
@include-previously-extracted["extracted.scrbl" "start-client"]
@include-previously-extracted["extracted.scrbl" "stop-client"]
@include-previously-extracted["extracted.scrbl" "update-status"]

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

@section{Data Models}

Note that these have rotted significantly at the time of writing. Please see
the source for precise details. Not all data models are fully specified or working.

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
  [members (hash/c string? member?)]
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

@defproc[(on-event [evt symbol?]
                   [client client?]
                   [callback procedure?]) void?]{
Subscribe to callbacks for the given gateway event identified by @racket[evt].

The Racket symbol name @racket[evt] corresponding to a given Discord gateway event is
derived as follows:

@itemlist[@item{Lowercase the event name}
          @item{Replace underscores with dashes}
          @item{Convert to symbol}]

For example, the Discord gateway event @tt{MESSAGE_CREATE} would be identified
as the Racket symbol @racket['message-create].

The exact type for @racket[callback] depends on the specific event, see @secref["typedevents"]
for more information.
}

Example:
@racketblock[
  (on-event 'message-create
   (lambda (client message)
     (println (message-content message))))
]

Each event has a raw counterpart, identified by prepending @tt{raw-} to the Racket
symbolic name. For example, the raw event symbol for @tt{MESSAGE_CREATE} would be
@racket['raw-message-create].

@italic{Raw events} contain the raw data as provided by the Discord gateway. In contrast,
the regular events are unmarshalled into structural representations by the library.

As of the time of writing, the regular events have bitrotted quite a bit and will need
some work to fix. If you must receive many events reliably, using raw events is recommended.

Raw events will also allow you to immediately react to new features added by
Discord without having to wait for the library to update.

In contrast to regular events, all raw event callbacks have the same type:

@defproc[(raw-callback [ws-client ws-client?]
                       [client client?]
                       [data jsexpr?]) void?]

where @racket[data] is the raw @tt{d} payload received from the Discord gateway.


@subsection[#:tag "typedevents"]{Event Callback Signatures}

Below are the expected callback signatures for event handlers.

Note that some of these are no longer accurate and the authors are working on fixing them.

@deftogether[(
  @defproc[(channel-create [client client?]
                           [channel (or/c dm-channel? guild-channel?)]) void?]
  @defproc[(channel-delete [client client?]
                           [channel (or/c dm-channel? guild-channel?)]) void?]
  @defproc[(channel-update [client client?]
                           [old-channel (or/c dm-channel? guild-channel?)]
                           [new-channel (or/c dm-channel? guild-channel?)]) void?]
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

@section{Miscellaneous functions}

@defproc[(get-channels [client client?]) (listof guild-channel?)]

@defproc[(get-channel [client client?]
                      [id string?]) (or/c guild-channel? null?)]{
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
                           (or/c dm-channel? guild-channel?)]{
Request a channel.
}

@defproc[(http:modify-channel [client client?]
                              [channel-id string?]
                              [data hash?])
                              (or/c dm-channel? guid-channel?)]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/channel#modify-channel"]{Modify Channel}.
}

@defproc[(http:delete-channel [client client?]
                              [channel-id string?]) jsexpr?]

@defproc[(http:get-channel-messages [client client?]
                                    [channel-id string?]
                                    [params (cons/c string? string?)] ...)
                                    (listof message?)]{
@racket[params] provided should be cons cells of @racket['(k . v)] conforming to @link["https://discordapp.com/developers/docs/resources/channel#get-channel-messages"]{Get Channel Message}.
}

@defproc[(http:get-channel-message [client client?]
                                   [channel-id string?]
                                   [message-id string?]) message?]

@defproc[(http:create-message [client client?]
                              [content string? ""]
                              [#:embed embed jsexpr? null]
                              [#:tts tts boolean? #f]
                              [#:file file attachment? #f]) message?]

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
@racket[params] provided should be cons cells of @racket['(k . v)] conforming to @link["https://discordapp.com/developers/docs/resources/channel#get-reactions"]{Get Reactions}.
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

@defproc[(http:get-guild [client client?]
                         [guild-id string?]) guild?]

@defproc[(http:modify-guild [client client?]
                            [guild-id string?]
                            [data hash?]) guild?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#modify-guild"]{Modify Guild}.
}

@defproc[(http:delete-guild [client client?]
                            [guild-id string?]) jsexpr?]

@defproc[(http:get-guild-channels [client client?]
                                  [guild-id string?]) (listof guild-channel?)]

@defproc[(http:create-guild-channel [client client?]
                                    [guild-id string?]
                                    [data hash?]) guild-channel?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#create-guild-channel"]{Create Guild Channel}.
}

@defproc[(http:modify-guild-channel-permissions [client client?]
                                                [guild-id string?]
                                                [data hash?])]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#modify-guild-channel-permissions"]{Modify Guild Channel Positions}.
}

@defproc[(http:get-guild-member [client client?]
                                [guild-id string?]
                                [user-id string?]) member?]

@defproc[(http:list-guild-members [client client?]
                                  [guild-id string?]
                                  [#:limit limit integer? 1]
                                  [#:after after integer? 0]) (listof member?)]

@defproc[(http:add-guild-member [client client?]
                                [guild-id string?]
                                [user-id string?]
                                [data hash?]) member?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#add-guild-member"]{Add Guild Member}.
}

@defproc[(http:modify-guild-member [client client?]
                                   [guild-id string?]
                                   [user-id string?]
                                   [data hash?]) jsexpr?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#modify-guild-member"]{Modify Guild Member}.
}

@defproc[(http:modify-user-nick [client client?]
                                [guild-id string?]
                                [nick string?]) jsexpr?]

@defproc[(http:add-guild-member-rols [client client?]
                                     [guild-id string?]
                                     [user-id string?]
                                     [role-id string?]) jsexpr?]

@defproc[(http:remove-guild-member-role [client client?]
                                        [guild-id string?]
                                        [user-id string?]
                                        [role-id string?]) jsexpr?]

@defproc[(http:remove-guild-member [client client?]
                                   [guild-id string?]
                                   [user-id string?]) jsexpr?]

@defproc[(http:get-guild-bans [client client?]
                              [guild-id string?]) jsexpr?]

@defproc[(http:create-guild-ban [client client?]
                                [guild-id string?]
                                [user-id string?]
                                [days integer? 1]) jsexpr?]

@defproc[(http:remove-guild-ban [client client?]
                                [guild-id string?]
                                [user-id string?]) jsexpr?]

@defproc[(http:get-guild-roles [client client?]
                               [guild-id string?]) (listof role?)]

@defproc[(http:create-guild-role [client client?]
                                 [guild-id string?]
                                 [data hash?]) role?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#create-guild-role"]{Create Guild Role}.
}

@defproc[(http:modify-guild-role-positions [client client?]
                                           [guild-id string?]
                                           [data hash?]) (listof role?)]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#modify-guild-role-positions"]{Modify Guild Role Positions}.
}

@defproc[(http:modify-guild-role [client client?]
                                 [guild-id string?]
                                 [role-id string?]
                                 [data hash?]) role?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#modify-guild-role"]{Modify Guild Role}.
}

@defproc[(http:delete-guild-role [client client?]
                                 [guild-id string?]
                                 [role-id string?]) jsexpr?]

@defproc[(http:get-guild-prune-count [client client?]
                                     [guild-id string?]
                                     [days integer?]) integer?]

@defproc[(http:begin-guild-prune [client client?]
                                 [guild-id string?]
                                 [days integer?]) integer?]

@defproc[(http:get-guild-invites [client client?]
                                 [guild-id string?]) (listof invite?)]

@defproc[(http:get-guild-integrations [client client?]
                                      [guild-id string?]) jsexpr?]

@defproc[(http:create-guild-integration [client client?]
                                        [guild-id string?]
                                        [type string?]
                                        [id string?]) jsexpr?]

@defproc[(http:modify-guild-integration [client client?]
                                        [guild-id string?]
                                        [integration-id string?]
                                        [data hash?]) jsexpr?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#modify-guild-integration"]{Modify Guild Integration}.
}

@defproc[(http:delete-guild-integration [client client?]
                                        [guild-id string?]
                                        [integration-id string?]) jsexpr?]

@defproc[(http:sync-guild-integrations [client client?]
                                       [guild-id string?]
                                       [integration-id string?]) jsexpr?]

@defproc[(http:get-guild-embed [client client?]
                               [guild-id string?]) jsexpr?]

@defproc[(http:modify-guild-embed [client client?]
                                  [guild-id string?]
                                  [data hash?]) jsexpr?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#modify-guild-embed"]{Modify Guild Embed}.
}

@defproc[(http:get-current-user [client client]) user?]

@defproc[(http:get-user [client client?]
                        [user-id string?]) user?]

@defproc[(http:modify-current-user [client client?]
                                   [#:username username string? null]
                                   [#:avatar avatar bytes? null]
                                   [#:avatar-type avatar-type string? ""]) user?]

@defproc[(http:get-current-user-guilds [client client?]
                                       [#:before before integer? null]
                                       [#:after after integer? null]
                                       [#:limit limit integer? null]) (listof guild?)]

@defproc[(http:leave-guild [client client?]
                           [guild-id string?]) jsexpr?]

@defproc[(http:get-user-dms [client client?]) (listof dm-channel?)]

@defproc[(http:create-dm [client client?]
                         [recipient-id string?]) dm-channel?]

@defproc[(http:create-group-dm [client client?]
                               [data hash?]) dm-channel?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/user#create-group-dm"]{Create Group DM}.
}

@defproc[(http:create-webhook [client client?]
                              [channel-id string?]
                              [name string?]
                              [avatar bytes?]
                              [avatar-type string?]) jsexpr?]

@defproc[(http:get-channel-webhooks [client client?]
                                    [channel-id string?]) jsexpr?]

@defproc[(http:get-guild-webhooks [client client?]
                                  [guild-id string?]) jsexpr?]

@defproc[(http:get-webhook [client client?]
                           [webhook-id string?]) jsexpr?]

@defproc[(http:get-webhook-with-token [client client?]
                                      [webhook-id string?]
                                      [webhook-token string?]) jsexpr?]

@defproc[(http:modify-webhook [client client?]
                              [webhook-id string?]
                              [#:name name string? null]
                              [#:avatar avatar bytes? null]
                              [#:avatar-type avatar-type string? ""]
                              [#:channel-id channel-id string? null]) jsexpr?]

@defproc[(http:modify-webhook-with-token [client client?]
                                         [webhook-id string?]
                                         [token string?]
                                         [#:name name string? null]
                                         [#:avatar avatar bytes null]
                                         [#:avatar-type avatar-type string ""]
                                         [#:channel-id channel-id string? null]) jsexpr?]

@defproc[(http:delete-webhook [client client?]
                              [webhook-id string?]) jsexpr?]

@defproc[(http:delete-webhook-with-token [client client?]
                                         [webhook-id string?]
                                         [webhook-token string?]) jsexpr?]

@defproc[(http:execute-webhook [client client?]
                               [webhook-id string?]
                               [webhook-token string?]
                               [data hash?]
                               [#:wait wait boolean? #f]) jsexpr?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/webhook#execute-webhook"]{Execute Webhook}.
}

@section{Exceptions}

@defstruct*[
  http:exn:fail:network:http:discord
  ([message string?]
  [continuation-marks continuation-mark-set?]
  [http-code number?]
  [discord-code number?]
  [reason string?])
  #:transparent]{
  Raised when a http error code is retrieved from discord.
}

@section{Additional}

@defthing[discord-logger logger?]{Logger of discord events in the library.}

@defstruct*[
  http:http-client
  ([requester requester?]
  [global-lock semaphore?]
  [ratelimits (hash/c string? semaphore?)])]{
Internal http client of the library, holds ratelimiting state.
}
