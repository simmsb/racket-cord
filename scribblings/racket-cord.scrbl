#lang scribble/manual
@require[scribble/extract]
@require[@for-label[racket-cord
                    json
                    net/rfc6455
                   @except-in[racket/base member]
				   racket/contract/base]]

@title{racket-cord: Racket discord library}
@author{@author+email["Ben Simms" "ben@bensimms.moe"]}

@defmodule[racket-cord]

@section{Quick Start}

@itemlist[@item{Create a client using @racket[make-client]}
          @item{Register websocket event handlers using @racket[on-event]}
          @item{Start the client and begin receiving events using @racket[start-client]}
		  @item{Make auxiliary HTTP calls using @seclink["http"]{provided utility functions}}]

@section{Client}

@include-previously-extracted["extracted.scrbl" "make-client"]
@include-previously-extracted["extracted.scrbl" "start-client"]
@include-previously-extracted["extracted.scrbl" "stop-client"]
@include-previously-extracted["extracted.scrbl" "update-status"]

@defstruct*[
  client
  ([shards list?]
  [shard-threads any/c]
  [user (hash/c symbol? any/c)]
  [events (hash/c symbol? procedure?)]
  [http-client any/c]
  [token string?]
  [intents any/c]
  [running semaphore?])
  #:mutable
  #:transparent]{
Stores the state of the client.
}

@section[#:tag "events"]{Events}

@defproc[(on-event [evt symbol?]
                   [client client?]
                   [callback procedure?]) void?]{
Subscribe to callbacks for the given gateway event identified by @racket[evt].

The Racket symbol name @racket[evt] corresponding to a given Discord gateway event is
derived as follows:

@itemlist[@item{Lowercase the event name}
          @item{Replace underscores with dashes}
          @item{Convert to symbol}
          @item{Prefix with "raw-"}]

For example, the Discord gateway event @tt{MESSAGE_CREATE} would be identified
as the Racket symbol @racket['raw-message-create].

The signature of @racket{callback} should be as follows.
}

@defproc[(raw-callback [ws-client ws-client?]
                       [client client?]
                       [data jsexpr?]) void?]

where @racket[data] is the raw @tt{d} payload received from the Discord gateway.

@section[#:tag "http"]{HTTP}

HTTP requests are defined here. Ratelimiting is handled for you by the library.
Requests that fail raise a @racket[exn:fail:network:http:discord?] exception.
All functions return the raw JSON as returned by the Discord REST API.

@defproc[(http:get-channel [client client?]
                           [channel-id string?])
                           jsexpr?]{
Request a channel.
}

@defproc[(http:modify-channel [client client?]
                              [channel-id string?]
                              [data hash?])
                              jsexpr?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/channel#modify-channel"]{Modify Channel}.
}

@defproc[(http:delete-channel [client client?]
                              [channel-id string?]) jsexpr?]

@defproc[(http:get-channel-messages [client client?]
                                    [channel-id string?]
                                    [params (cons/c string? string?)] ...)
                                    jsexpr?]{
@racket[params] provided should be cons cells of @racket['(k . v)] conforming to @link["https://discordapp.com/developers/docs/resources/channel#get-channel-messages"]{Get Channel Message}.
}

@defproc[(http:get-channel-message [client client?]
                                   [channel-id string?]
                                   [message-id string?]) jsexpr?]

@defproc[(http:create-message [client client?]
                              [content string? ""]
                              [#:embed embed jsexpr? null]
                              [#:tts tts boolean? #f]
                              [#:file file attachment? #f]) jsexpr?]

@defproc[(http:edit-message [client client?]
                            [channel-id string?]
                            [message-id string?]
                            [#:content content (or/c string? null?) null]
                            [#:embed embed jsexpr? null]) jsexpr?]

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
                             jsexpr?]{
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
                                     [unique boolean #f]) jsexpr?]

@defproc[(http:delete-channel-permission [client client?]
                                         [channel-id string?]
                                         [overwrite-id string?]) jsexpr?]

@defproc[(http:trigger-typing-indicator [client client?]
                                        [channel-id string?]) jsexpr?]

@defproc[(http:get-pinned-messages [client client?]
                                   [channel-id string?]) jsexpr?]

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
                                [guild-id string?]) jsexpr?]

@defproc[(http:get-guild-emoji [client client?]
                               [guild-id string?]
                               [emoji-id string?]) jsexpr?]

@defproc[(http:create-guild-emoji [client client?]
                                  [guild-id string?]
                                  [name string?]
                                  [image bytes?]
                                  [image-type string?]
                                  [roles (listof string?)]) jsexpr?]

@defproc[(http:modify-guild-emoji [client client?]
                                  [guild-id string?]
                                  [emoji-id string?]
                                  [name string?]
                                  [roles (listof string?)]) jsexpr?]

@defproc[(http:delete-guild-emoji [client client?]
                                  [guild-id string?]
                                  [emoji-id string?]) jsexpr?]

@defproc[(http:get-guild [client client?]
                         [guild-id string?]) jsexpr?]

@defproc[(http:modify-guild [client client?]
                            [guild-id string?]
                            [data hash?]) jsexpr?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#modify-guild"]{Modify Guild}.
}

@defproc[(http:delete-guild [client client?]
                            [guild-id string?]) jsexpr?]

@defproc[(http:get-guild-channels [client client?]
                                  [guild-id string?]) jsexpr?]

@defproc[(http:create-guild-channel [client client?]
                                    [guild-id string?]
                                    [data hash?]) jsexpr?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#create-guild-channel"]{Create Guild Channel}.
}

@defproc[(http:modify-guild-channel-permissions [client client?]
                                                [guild-id string?]
                                                [data hash?])]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#modify-guild-channel-permissions"]{Modify Guild Channel Positions}.
}

@defproc[(http:get-guild-member [client client?]
                                [guild-id string?]
                                [user-id string?]) jsexpr?]

@defproc[(http:list-guild-members [client client?]
                                  [guild-id string?]
                                  [#:limit limit integer? 1]
                                  [#:after after integer? 0]) jsexpr?]

@defproc[(http:add-guild-member [client client?]
                                [guild-id string?]
                                [user-id string?]
                                [data hash?]) jsexpr?]{
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
                               [guild-id string?]) jsexpr?]

@defproc[(http:create-guild-role [client client?]
                                 [guild-id string?]
                                 [data hash?]) jsexpr?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#create-guild-role"]{Create Guild Role}.
}

@defproc[(http:modify-guild-role-positions [client client?]
                                           [guild-id string?]
                                           [data hash?]) jsexpr?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#modify-guild-role-positions"]{Modify Guild Role Positions}.
}

@defproc[(http:modify-guild-role [client client?]
                                 [guild-id string?]
                                 [role-id string?]
                                 [data hash?]) jsexpr?]{
@racket[data] should be a hashmap that conforms to @link["https://discordapp.com/developers/docs/resources/guild#modify-guild-role"]{Modify Guild Role}.
}

@defproc[(http:delete-guild-role [client client?]
                                 [guild-id string?]
                                 [role-id string?]) jsexpr?]

@defproc[(http:get-guild-prune-count [client client?]
                                     [guild-id string?]
                                     [days integer?]) jsexpr?]

@defproc[(http:begin-guild-prune [client client?]
                                 [guild-id string?]
                                 [days integer?]) jsexpr?]

@defproc[(http:get-guild-invites [client client?]
                                 [guild-id string?]) jsexpr?]

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

@defproc[(http:get-current-user [client client]) jsexpr?]

@defproc[(http:get-user [client client?]
                        [user-id string?]) jsexpr?]

@defproc[(http:modify-current-user [client client?]
                                   [#:username username string? null]
                                   [#:avatar avatar bytes? null]
                                   [#:avatar-type avatar-type string? ""]) jsexpr?]

@defproc[(http:get-current-user-guilds [client client?]
                                       [#:before before integer? null]
                                       [#:after after integer? null]
                                       [#:limit limit integer? null]) jsexpr?]

@defproc[(http:leave-guild [client client?]
                           [guild-id string?]) jsexpr?]

@defproc[(http:get-user-dms [client client?]) jsexpr?]

@defproc[(http:create-dm [client client?]
                         [recipient-id string?]) jsexpr?]

@defproc[(http:create-group-dm [client client?]
                               [data hash?]) jsexpr?]{
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
