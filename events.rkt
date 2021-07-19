#lang racket/base

(require racket/function
         racket/match
         racket/set
         racket/string
         "data.rkt"
         "utils.rkt")

(provide add-events
         get-channels
         get-channel
         get-guild
         get-member
         on-event
         dispatch-event)

;; TODO: Client needs to be made an object and these made methods of it
(define (get-channels client)
  (extract-merge guild-channels append (hash-values (client-guilds client))))

(define (get-channel client id)
  (find-key (get-channels client) id))

(define (get-guild client id)
  (hash-ref (client-guilds client) id null))

(define (get-member client member-id guild-id)
  (bindap hash-ref (bind guild-members (get-guild client guild-id))
          (list member-id null)))

(define (member-hash-id member)
  (hash-ref (hash-ref member 'user) 'id))

(define (dispatch-event ws-client data type)
  (thread
   (thunk
    (log-discord-debug "DISPATCHING EVENT: ~a" type)
    (with-handlers ([exn:fail?
                     (lambda (v)
                       (log-discord-warning "Event ~a ERRORED with: ~a" type v))])
      (let ([client (ws-client-client ws-client)]
            [events (client-events (ws-client-client ws-client))]
            [raw-evt (string->symbol (string-downcase (format "raw-~a" (string-replace type "_" "-"))))]
            [evt (string->symbol (string-downcase (string-replace type "_" "-")))])
        (match (hash-ref events raw-evt null)
          [(? null?) null]
          [funs (each funs ws-client client data)]))))))

(define (on-event evt client callback)
  (let ([events (client-events client)])
    (match (hash-ref events evt null)
      [(? null?) (hash-set! events evt (mutable-set callback))]
      [x (set-add! x callback)])))

(define (add-events client)
  (on-event 'raw-ready client event-ready)
  (on-event 'raw-guild-create client event-guild-create)
  (on-event 'raw-guild-update client event-guild-update)
  (on-event 'raw-guild-delete client event-guild-delete)
  (on-event 'raw-channel-create client event-channel-create)
  (on-event 'raw-channel-update client event-channel-update)
  (on-event 'raw-channel-delete client event-channel-delete)
  (on-event 'raw-emojis-update client event-emojis-update)
  (on-event 'raw-guild-member-add client event-guild-member-add)
  (on-event 'raw-guild-member-remove client event-guild-member-remove)
  (on-event 'raw-guild-member-update client event-guild-member-update)
  (on-event 'raw-guild-members-chunk client event-guild-members-chunk)
  (on-event 'raw-guild-role-create client event-guild-role-create)
  (on-event 'raw-guild-role-delete client event-guild-role-delete)
  (on-event 'raw-guild-role-update client event-guild-role-update)
  (on-event 'raw-presence-update client event-guild-member-update)
  (on-event 'raw-user-update client event-user-update))

(define (event-ready ws-client client data)
  (let ([session-id (hash-ref data 'session_id)]
        [guilds (hash-ref data 'guilds)]
        [user (hash-ref data 'user)])
    (set-ws-client-session-id! ws-client session-id)
    (when (null? (client-user client))
      (set-client-user! client (hash->user user)))
    (for ([guild-obj guilds])
      (hash-set! (client-guilds client) (hash-ref guild-obj 'id)
                 (hash->guild (ws-client-shard-id ws-client) guild-obj))) ;; these are only partials idk
    ))

(define (event-guild-create ws-client client data)
  (hash-set! (client-guilds client)
             (hash-ref data 'id) (hash->guild (ws-client-shard-id ws-client) data)))

(define (event-guild-update ws-client client data)
  (let ([old-guild (get-guild client (hash-ref data 'id))])
    (hash-set! (client-guilds client)
               (hash-ref data 'id) (update-guild old-guild data))))

(define (event-guild-delete ws-client client data)
  (unless (hash-has-key? data 'unavailable)
    (hash-remove! (client-guilds client)
                  (hash-ref data 'id))))

(define (event-channel-create ws-client client data)
  (let ([id (hash-ref data 'id)]
        [chan (hash->channel data)])
    (case (hash-ref data 'type)
      [(1 3) (hash-set! (client-private-channels client) id chan)]
      [else (let ([guild (get-guild client (hash-ref data 'guild_id))])
              (hash-set! (guild-channels guild) id chan))])))

(define (event-channel-update ws-client client data)
  (let ([guild (get-guild client (hash-ref data 'guild_id))])
    (hash-update! (guild-channels guild)
                  (hash-ref data 'guild_id) (lambda (old) (update-channel old data)))))

(define (event-channel-delete ws-client client data)
  (let ([id (hash-ref data 'id)]
        [client (ws-client-client ws-client)])
    (case (hash-ref data 'type)
      [(1 3) (hash-remove! (client-private-channels client) id)]
      [else (let ([guild (get-guild client (hash-ref data 'guild_id))])
              (hash-remove! (guild-channels guild) id))])))

(define (event-emojis-update ws-client client data)
  (set-guild-emojis! (get-guild client (hash-ref data 'guild_id))
                     (map hash->emoji (hash-ref data 'emojis))))

(define (event-guild-member-add ws-client client data)
  (let ([guild (get-guild client (hash-ref data 'guild_id))])
    (hash-set! (guild-members guild) (member-hash-id data)
               (hash->guild-member data))))

(define (event-guild-member-remove ws-client client data)
  (let ([guild (get-guild client (hash-ref data 'guild_id))])
    (hash-remove! (guild-members guild) (member-hash-id data))))

(define (event-guild-member-update ws-client client data)
  (let ([guild (get-guild client (hash-ref data 'guild_id))]
        [id (member-hash-id data)])
    (hash-set! (guild-members guild) id
               (update-member (hash-ref (guild-members guild) id) data))))

(define (event-guild-members-chunk ws-client client data)
  (let ([guild (get-guild client (hash-ref data 'guild_id))])
    (for ([i (hash-ref data 'members)])
      (hash-set! (guild-members guild) (member-hash-id i)
                 (hash->guild-member i)))))

(define (event-guild-role-create ws-client client data)
  (let ([guild (get-guild client (hash-ref data 'guild_id))]
        [role (hash->role (hash-ref data 'role))])
    (hash-set! (guild-roles guild) (role-id role) role)))

(define (event-guild-role-delete ws-client client data)
  (let ([guild (get-guild client (hash-ref data 'guild_id))])
    (hash-remove! (guild-roles guild) (hash-ref data 'role_id))))

(define (event-guild-role-update ws-client client data)
  (let ([guild (get-guild client (hash-ref data 'guild_id))])
    (hash-set! (guild-roles guild) (hash->role (hash-ref data 'role)))))

(define (event-user-update ws-client client data)
  (set-client-user! (update-user (client-user client) data)))
