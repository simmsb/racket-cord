#lang racket/base

(provide (all-defined-out))

; Sample data for unit testing, derived from Discord API docs

(define user-example
  #<<END
{
  "id": "80351110224678912",
  "username": "Nelly",
  "discriminator": "1337",
  "avatar": "8342729096ea3675442027381ff50dfe",
  "verified": true,
  "email": "nelly@discord.com",
  "flags": 64,
  "premium_type": 1,
  "public_flags": 64
}
END
  )

(define member-example
  #<<END
{
  "user": {},
  "nick": "NOT API SUPPORT",
  "roles": [],
  "joined_at": "2015-04-26T06:26:56.936000+00:00",
  "deaf": false,
  "mute": false
}
END
  )

(define webhook-example
  #<<END
{
  "name": "test webhook",
  "type": 1,
  "channel_id": "199737254929760256",
  "token": "3d89bb7572e0fb30d8128367b3b1b44fecd1726de135cbe28a41f8b2f777c372ba2939e72279b94526ff5d1bd4358d65cf11",
  "avatar": null,
  "guild_id": "199737254929760256",
  "id": "223704706495545344",
  "user": {
    "username": "test",
    "discriminator": "7479",
    "id": "190320984123768832",
    "avatar": "b004ec1740a63ca06ae2e14c5cee11f3"
  }
}
END
  )

(define emoji-example
  #<<END
{
  "id": "41771983429993937",
  "name": "LUL",
  "roles": ["41771983429993000", "41771983429993111"],
  "user": {
    "username": "Luigi",
    "discriminator": "0002",
    "id": "96008815106887111",
    "avatar": "5500909a3274e1812beb4e8de6631111"
  },
  "require_colons": true,
  "managed": false,
  "animated": false
}
END
  )

(define emoji-gateway-standard-example
  #<<END
{
  "id": null,
  "name": "🔥"
}
END
  )

(define emoji-gateway-custom-example
  #<<END
{
  "id": "41771983429993937",
  "name": "LUL",
  "animated": true
}
END
  )

(define emoji-gateway-custom-example-2
  #<<END
{
  "id": "41771983429993937",
  "name": null
}
END
  )

(define role-example
  #<<END
{
  "id": "41771983423143936",
  "name": "WE DEM BOYZZ!!!!!!",
  "color": 3447003,
  "hoist": true,
  "position": 1,
  "permissions": "66321471",
  "managed": false,
  "mentionable": false
}
END
  )

(define invite-example
  #<<END
{
  "code": "0vCdhLbwjZZTWZLD",
  "guild": {
    "id": "165176875973476352",
    "name": "CS:GO Fraggers Only",
    "splash": null,
    "banner": null,
    "description": "Very good description",
    "icon": null,
    "features": ["NEWS", "DISCOVERABLE"],
    "verification_level": 2,
    "vanity_url_code": null
  },
  "channel": {
    "id": "165176875973476352",
    "name": "illuminati",
    "type": 0
  },
  "inviter": {
    "id": "115590097100865541",
    "username": "speed",
    "avatar": "deadbeef",
    "discriminator": "7653"
  },
  "target_user": {
    "id": "165176875973476352",
    "username": "bob",
    "avatar": "deadbeef",
    "discriminator": "1234"
  },
  "target_user_type": 1
}
END
  )

(define message-example
  #<<END
{
  "reactions": [
    {
      "count": 1,
      "me": false,
      "emoji": {
        "id": null,
        "name": "🔥"
      }
    }
  ],
  "attachments": [],
  "tts": false,
  "embeds": [],
  "timestamp": "2017-07-11T17:27:07.299000+00:00",
  "mention_everyone": false,
  "id": "334385199974967042",
  "pinned": false,
  "edited_timestamp": null,
  "author": {
    "username": "Mason",
    "discriminator": "9999",
    "id": "53908099506183680",
    "avatar": "a_bab14f271d565501444b2ca3be944b25"
  },
  "mention_roles": [],
  "content": "Supa Hot",
  "channel_id": "290926798999357250",
  "mentions": [],
  "type": 0
}
END
  )

(define message-crosspost-example
  #<<END
{
  "reactions": [
    {
      "count": 1,
      "me": false,
      "emoji": {
        "id": null,
        "name": "🔥"
      }
    }
  ],
  "attachments": [],
  "tts": false,
  "embeds": [],
  "timestamp": "2017-07-11T17:27:07.299000+00:00",
  "mention_everyone": false,
  "id": "334385199974967042",
  "pinned": false,
  "edited_timestamp": null,
  "author": {
    "username": "Mason",
    "discriminator": "9999",
    "id": "53908099506183680",
    "avatar": "a_bab14f271d565501444b2ca3be944b25"
  },
  "mention_roles": [],
  "mention_channels": [
    {
      "id": "278325129692446722",
      "guild_id": "278325129692446720",
      "name": "big-news",
      "type": 5
    }
  ],
  "content": "Big news! In this <#278325129692446722> channel!",
  "channel_id": "290926798999357250",
  "mentions": [],
  "type": 0,
  "flags": 2,
  "message_reference": {
    "channel_id": "278325129692446722",
    "guild_id": "278325129692446720",
    "message_id": "306588351130107906"
  }
}
END
  )
