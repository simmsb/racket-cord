#lang racket

(require "data.rkt"
         "utils.rkt")

(provide event-ready
         event-guild-create
         dispatch-event
         event-consumer
         add-events)

(define (event-ready ws-client data)
  (let ([client (ws-client-client ws-client)]
        [session-id (hash-ref data 'session_id)]
        [private-channels (hash-ref data 'private_channels)]
        [guilds (hash-ref data 'guilds)]
        [user (hash-ref data 'user)])
    (set-ws-client-session-id! ws-client session-id)
    (when (null? (client-user client))
      (set-client-user! client (hash->user user)))
    (for ([guild-obj guilds])
      (hash-set! (client-guilds client) (hash-ref guild-obj 'id)
                 (hash->guild (ws-client-shard-id ws-client) guild-obj))) ;; these are only partials idk
    (for ([priv private-channels])
      (hash-set! (client-private-channels client) (hash-ref priv 'id) (hash->channel priv)))))

(define (event-guild-create ws-client data) ;; Should we replace the object or merge the changes?
  (hash-set! (client-guilds (ws-client-client ws-client))
             (hash-ref data 'id) (hash->guild (ws-client-shard-id ws-client) data)))

(define (event-guild-delete ws-client data) ;; TODO: handle unavailable properly
  (hash-remove! (client-guilds (ws-client-client ws-client))
                (hash-ref data 'id)))

(define (get-channels client)
  (extract-merge guild-channels append (hash-values (client-guilds client))))

(define (get-channel client id)
  (find-key (get-channels client) id))

(define (get-guild client id)
  (hash-ref (client-guilds client) id null))

(define (get-member client member-id guild-id)
  (hash-ref (guild-members (get-guild client guild-id))
            member-id))

(define (update-member old-member new-roles new-user new-nick)
  (struct-copy member old-member
               [roles new-roles]
               [user new-user]
               [nick new-nick]))

(define (dispatch-event ws-client data type)
  (printf "DISPATCHING EVENT: ~a\n" type)
  (let ([client (ws-client-client ws-client)]
        [events (client-events (ws-client-client ws-client))]
        [raw-evt (string->symbol (string-downcase (format "raw-~a" (string-replace type "_" "-"))))]
        [evt (string->symbol (string-downcase (string-replace type "_" "-")))])
    (match (hash-ref events evt null)
      [(? null?) null]
      [funs
       (case evt
         [(ready) (each funs client (ws-client-shard-id ws-client))] ;; TODO: delay this until we have all guilds for this shard
         [(channel-create channel-delete) (each funs client (hash->channel data))]
         [(channel-update)
          (let ([old-channel (get-channel client (hash-ref data 'id))]
                [new-channel (hash->channel data)])
            (each funs client old-channel new-channel))]
         [(guild-create guild-delete) (each funs client (hash->guild (ws-client-shard-id ws-client) data))]
         [(guild-update)
          (let ([old-guild (get-guild (hash-ref data 'id))]
                [new-guild (hash->guild (ws-client-shard-id ws-client) data)])
            (each funs client old-guild new-guild))]
         [(guild-ban-add client-ban-remove)
          (each funs client (hash->user data)
                (get-guild (hash-ref data 'guild_id)))]
         [(guild-emojis-update) null] ;; TODO: add this after writing emoji stuff
         [(guild-member-add)
          (each funs client (hash->member data)
                (get-guild (hash-ref data 'guild_id)))]
         [(guild-member-remove) ;; get guild object, get member object from guild object
          (each funs client (get-member client (hash-ref (hash-ref data 'user) 'id) (hash-ref data 'guild_id)))]
         [(guild-member-update)
          (let ([old-member (get-member client (hash-ref (hash-ref data 'user) 'id) (hash-ref data 'guild_id))])
            (each funs client old-member
                  (update-member old-member (hash-ref data 'roles) (hash->user (hash-ref data 'user)) (hash-ref data 'nick))))]
         [(message-create) (each funs client (hash->message data))]
         ;;[(message-delete) (each funs client ())] TODO: cache messages
         [(message-reaction-add message-reaction-remove) #t] ;; TODO
         [(presence-update) #t] ;; TODO
         [(typing-start)
          (let ([channel (get-channel client (hash-ref data 'channel_id))])
            (let ([member (get-member client (hash-ref data 'user_id) (channel-guild-id channel))])
              (each funs client channel member)))])])
    (match (hash-ref events raw-evt null) ;; Apply raw events TODO: move this to after non-raw events
      [(? null?) null]
      [funs (each funs ws-client data)])))

(define (event-consumer)
  (thread
   (thunk
    (let loop ()
      (with-handlers ([(const #t) (lambda (v)
                                    (printf "EVENT CONSUMER ERRORED WITH: ~a\n" v)
                                    (loop))])
        (match (thread-receive)
          [(list ws data type)
           (dispatch-event ws data type)
           (loop)]
          ['done null]))))))

(define (on-event evt client callback)
  (let ([events (client-events client)])
    (match (hash-ref events evt null)
      [(? null?) (hash-set! events evt (mutable-set callback))]
      [x (set-add! x callback)])))

(define (add-events client)
  (on-event 'raw-ready client event-ready)
  (on-event 'raw-guild-create client event-guild-create)
  (on-event 'raw-guild-delete client event-guild-delete))
