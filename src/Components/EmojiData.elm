{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Components.EmojiData exposing (Emoji, searchEmojis)


type alias Emoji =
    { unicode : String, name : String, keywords : String }


searchEmojis : Maybe Int -> String -> List Emoji
searchEmojis maxResults pattern =
    let
        take n list =
            case n of
                Just limit ->
                    List.take limit list

                Nothing ->
                    list
    in
    if String.isEmpty pattern then
        take maxResults allEmojis

    else
        let
            lowerPattern =
                String.toLower pattern
        in
        allEmojis
            |> List.filter (\e -> String.contains lowerPattern (e.name ++ " " ++ e.keywords))
            |> take maxResults


allEmojis : List Emoji
allEmojis =
    -- Smileys & Emotion
    [ { unicode = "😀", name = "grinning", keywords = "smile happy face" }
    , { unicode = "😃", name = "smiley", keywords = "smile happy face" }
    , { unicode = "😄", name = "smile", keywords = "happy joy face" }
    , { unicode = "😁", name = "grin", keywords = "smile happy face" }
    , { unicode = "😆", name = "laughing", keywords = "smile happy lol" }
    , { unicode = "😅", name = "sweat-smile", keywords = "hot nervous laugh" }
    , { unicode = "🤣", name = "rofl", keywords = "laugh lol rolling" }
    , { unicode = "😂", name = "joy", keywords = "laugh cry happy tears" }
    , { unicode = "🙂", name = "slightly-smiling", keywords = "smile" }
    , { unicode = "🙃", name = "upside-down", keywords = "silly sarcasm" }
    , { unicode = "😉", name = "wink", keywords = "flirt" }
    , { unicode = "😊", name = "blush", keywords = "smile happy shy" }
    , { unicode = "😇", name = "innocent", keywords = "angel halo" }
    , { unicode = "🥰", name = "smiling-hearts", keywords = "love adore" }
    , { unicode = "😍", name = "heart-eyes", keywords = "love crush" }
    , { unicode = "🤩", name = "star-struck", keywords = "excited wow" }
    , { unicode = "😘", name = "kissing-heart", keywords = "love flirt" }
    , { unicode = "😗", name = "kissing", keywords = "kiss love" }
    , { unicode = "😚", name = "kissing-closed-eyes", keywords = "love kiss" }
    , { unicode = "😙", name = "kissing-smiling-eyes", keywords = "love kiss" }
    , { unicode = "🥲", name = "smiling-tear", keywords = "sad happy" }
    , { unicode = "😋", name = "yum", keywords = "delicious tongue" }
    , { unicode = "😛", name = "stuck-out-tongue", keywords = "silly playful" }
    , { unicode = "😜", name = "stuck-out-tongue-winking", keywords = "silly playful" }
    , { unicode = "🤪", name = "zany", keywords = "crazy wild silly" }
    , { unicode = "😝", name = "stuck-out-tongue-closed-eyes", keywords = "silly playful" }
    , { unicode = "🤑", name = "money-mouth", keywords = "rich dollar" }
    , { unicode = "🤗", name = "hugging", keywords = "hug love warm" }
    , { unicode = "🤭", name = "hand-over-mouth", keywords = "oops giggle" }
    , { unicode = "🤫", name = "shushing", keywords = "quiet secret" }
    , { unicode = "🤔", name = "thinking", keywords = "hmm wonder consider" }
    , { unicode = "🤐", name = "zipper-mouth", keywords = "secret quiet" }
    , { unicode = "🤨", name = "raised-eyebrow", keywords = "skeptical suspicious" }
    , { unicode = "😐", name = "neutral", keywords = "meh indifferent" }
    , { unicode = "😑", name = "expressionless", keywords = "blank meh" }
    , { unicode = "😶", name = "no-mouth", keywords = "silent mute" }
    , { unicode = "😏", name = "smirk", keywords = "smug sly" }
    , { unicode = "😒", name = "unamused", keywords = "meh annoyed" }
    , { unicode = "🙄", name = "eye-roll", keywords = "bored annoyed whatever" }
    , { unicode = "😬", name = "grimacing", keywords = "awkward nervous" }
    , { unicode = "🤥", name = "lying", keywords = "pinocchio liar" }
    , { unicode = "😌", name = "relieved", keywords = "relaxed peaceful" }
    , { unicode = "😔", name = "pensive", keywords = "sad thoughtful" }
    , { unicode = "😪", name = "sleepy", keywords = "tired rest" }
    , { unicode = "🤤", name = "drooling", keywords = "hungry want" }
    , { unicode = "😴", name = "sleeping", keywords = "tired zzz rest" }
    , { unicode = "😷", name = "mask", keywords = "sick ill doctor" }
    , { unicode = "🤒", name = "thermometer", keywords = "sick ill fever" }
    , { unicode = "🤕", name = "bandage", keywords = "hurt injured" }
    , { unicode = "🤢", name = "nauseated", keywords = "sick gross vomit" }
    , { unicode = "🤮", name = "vomiting", keywords = "sick gross" }
    , { unicode = "🤧", name = "sneezing", keywords = "sick cold" }
    , { unicode = "🥵", name = "hot", keywords = "heat sweat warm" }
    , { unicode = "🥶", name = "cold", keywords = "freeze frozen" }
    , { unicode = "🥴", name = "woozy", keywords = "dizzy drunk" }
    , { unicode = "😵", name = "dizzy", keywords = "confused spiral" }
    , { unicode = "🤯", name = "exploding-head", keywords = "mind blown shocked" }
    , { unicode = "🤠", name = "cowboy", keywords = "hat western" }
    , { unicode = "🥳", name = "partying", keywords = "celebrate birthday party" }
    , { unicode = "🥸", name = "disguised", keywords = "nose glasses" }
    , { unicode = "😎", name = "sunglasses", keywords = "cool" }
    , { unicode = "🤓", name = "nerd", keywords = "glasses geek smart" }
    , { unicode = "🧐", name = "monocle", keywords = "inspect curious" }
    , { unicode = "😕", name = "confused", keywords = "unsure puzzled" }
    , { unicode = "😟", name = "worried", keywords = "concern nervous" }
    , { unicode = "🙁", name = "slightly-frowning", keywords = "sad disappointed" }
    , { unicode = "😮", name = "open-mouth", keywords = "surprise wow" }
    , { unicode = "😯", name = "hushed", keywords = "surprise quiet" }
    , { unicode = "😲", name = "astonished", keywords = "surprise shocked" }
    , { unicode = "😳", name = "flushed", keywords = "embarrassed shy" }
    , { unicode = "🥺", name = "pleading", keywords = "puppy eyes beg" }
    , { unicode = "😦", name = "frowning-open-mouth", keywords = "sad aw" }
    , { unicode = "😧", name = "anguished", keywords = "stunned sad" }
    , { unicode = "😨", name = "fearful", keywords = "scared afraid" }
    , { unicode = "😰", name = "anxious-sweat", keywords = "nervous worried" }
    , { unicode = "😥", name = "sad-relieved", keywords = "cry disappointed" }
    , { unicode = "😢", name = "cry", keywords = "sad tear" }
    , { unicode = "😭", name = "sob", keywords = "cry sad tears" }
    , { unicode = "😱", name = "scream", keywords = "fear horror" }
    , { unicode = "😖", name = "confounded", keywords = "quiver frustrated" }
    , { unicode = "😣", name = "persevere", keywords = "struggle endure" }
    , { unicode = "😞", name = "disappointed", keywords = "sad" }
    , { unicode = "😓", name = "downcast-sweat", keywords = "sad cold" }
    , { unicode = "😩", name = "weary", keywords = "tired frustrated" }
    , { unicode = "😫", name = "tired", keywords = "exhausted" }
    , { unicode = "🥱", name = "yawning", keywords = "bored sleepy tired" }
    , { unicode = "😤", name = "triumph", keywords = "steam proud angry" }
    , { unicode = "😡", name = "rage", keywords = "angry mad" }
    , { unicode = "😠", name = "angry", keywords = "mad frustrated" }
    , { unicode = "🤬", name = "cursing", keywords = "swear angry" }
    , { unicode = "😈", name = "smiling-imp", keywords = "devil evil" }
    , { unicode = "👿", name = "imp", keywords = "devil angry evil" }
    , { unicode = "💀", name = "skull", keywords = "death dead skeleton" }
    , { unicode = "☠️", name = "skull-crossbones", keywords = "death danger" }
    , { unicode = "💩", name = "poop", keywords = "poo" }
    , { unicode = "🤡", name = "clown", keywords = "face funny" }
    , { unicode = "👹", name = "ogre", keywords = "monster japanese" }
    , { unicode = "👻", name = "ghost", keywords = "halloween spooky" }
    , { unicode = "👽", name = "alien", keywords = "ufo space" }
    , { unicode = "👾", name = "space-invader", keywords = "game alien" }
    , { unicode = "🤖", name = "robot", keywords = "bot machine" }
    , { unicode = "😺", name = "smiley-cat", keywords = "happy face" }
    , { unicode = "😸", name = "smile-cat", keywords = "happy face" }
    , { unicode = "😹", name = "joy-cat", keywords = "happy tears" }
    , { unicode = "😻", name = "heart-eyes-cat", keywords = "love" }
    , { unicode = "🙈", name = "see-no-evil", keywords = "monkey blind" }
    , { unicode = "🙉", name = "hear-no-evil", keywords = "monkey deaf" }
    , { unicode = "🙊", name = "speak-no-evil", keywords = "monkey quiet" }

    -- Gestures & People
    , { unicode = "👋", name = "wave", keywords = "hello hi bye hand" }
    , { unicode = "🤚", name = "raised-back-hand", keywords = "stop" }
    , { unicode = "🖐️", name = "hand-splayed", keywords = "five fingers" }
    , { unicode = "✋", name = "raised-hand", keywords = "stop high five" }
    , { unicode = "🖖", name = "vulcan", keywords = "spock trek" }
    , { unicode = "👌", name = "ok-hand", keywords = "perfect fine" }
    , { unicode = "🤌", name = "pinched-fingers", keywords = "italian" }
    , { unicode = "🤏", name = "pinching-hand", keywords = "small tiny" }
    , { unicode = "✌️", name = "victory", keywords = "peace two" }
    , { unicode = "🤞", name = "crossed-fingers", keywords = "luck hope" }
    , { unicode = "🤟", name = "love-you", keywords = "ily hand" }
    , { unicode = "🤘", name = "metal", keywords = "rock horns" }
    , { unicode = "🤙", name = "call-me", keywords = "phone hang loose" }
    , { unicode = "👈", name = "point-left", keywords = "direction" }
    , { unicode = "👉", name = "point-right", keywords = "direction" }
    , { unicode = "👆", name = "point-up", keywords = "direction" }
    , { unicode = "👇", name = "point-down", keywords = "direction" }
    , { unicode = "☝️", name = "point-up-2", keywords = "direction one" }
    , { unicode = "👍", name = "thumbsup", keywords = "yes good approve like" }
    , { unicode = "👎", name = "thumbsdown", keywords = "no bad dislike" }
    , { unicode = "✊", name = "fist", keywords = "punch power" }
    , { unicode = "👊", name = "punch", keywords = "fist bump" }
    , { unicode = "🤛", name = "left-fist", keywords = "fist bump" }
    , { unicode = "🤜", name = "right-fist", keywords = "fist bump" }
    , { unicode = "👏", name = "clap", keywords = "applause bravo" }
    , { unicode = "🙌", name = "raised-hands", keywords = "celebrate hooray" }
    , { unicode = "👐", name = "open-hands", keywords = "hug" }
    , { unicode = "🤲", name = "palms-up", keywords = "prayer cupped" }
    , { unicode = "🤝", name = "handshake", keywords = "deal agreement" }
    , { unicode = "🙏", name = "pray", keywords = "thanks please hope" }
    , { unicode = "✍️", name = "writing-hand", keywords = "write" }
    , { unicode = "💪", name = "muscle", keywords = "strong flex bicep" }
    , { unicode = "🦾", name = "mechanical-arm", keywords = "prosthetic robot" }
    , { unicode = "🧠", name = "brain", keywords = "smart think mind" }
    , { unicode = "👀", name = "eyes", keywords = "look see watch" }
    , { unicode = "👁️", name = "eye", keywords = "look see" }
    , { unicode = "👅", name = "tongue", keywords = "taste lick" }
    , { unicode = "👄", name = "lips", keywords = "mouth kiss" }

    -- Hearts & Symbols
    , { unicode = "❤️", name = "red-heart", keywords = "love like" }
    , { unicode = "🧡", name = "orange-heart", keywords = "love" }
    , { unicode = "💛", name = "yellow-heart", keywords = "love" }
    , { unicode = "💚", name = "green-heart", keywords = "love" }
    , { unicode = "💙", name = "blue-heart", keywords = "love" }
    , { unicode = "💜", name = "purple-heart", keywords = "love" }
    , { unicode = "🖤", name = "black-heart", keywords = "love dark" }
    , { unicode = "🤍", name = "white-heart", keywords = "love pure" }
    , { unicode = "🤎", name = "brown-heart", keywords = "love" }
    , { unicode = "💔", name = "broken-heart", keywords = "sad love" }
    , { unicode = "❣️", name = "heart-exclamation", keywords = "love" }
    , { unicode = "💕", name = "two-hearts", keywords = "love" }
    , { unicode = "💞", name = "revolving-hearts", keywords = "love" }
    , { unicode = "💓", name = "heartbeat", keywords = "love pulse" }
    , { unicode = "💗", name = "heartpulse", keywords = "love growing" }
    , { unicode = "💖", name = "sparkling-heart", keywords = "love" }
    , { unicode = "💘", name = "cupid", keywords = "love arrow" }
    , { unicode = "💝", name = "gift-heart", keywords = "love ribbon" }
    , { unicode = "💟", name = "heart-decoration", keywords = "love" }
    , { unicode = "💯", name = "100", keywords = "hundred perfect score" }
    , { unicode = "💢", name = "anger", keywords = "angry symbol" }
    , { unicode = "💥", name = "boom", keywords = "collision explosion" }
    , { unicode = "💫", name = "dizzy-star", keywords = "sparkle star" }
    , { unicode = "💦", name = "sweat-drops", keywords = "water" }
    , { unicode = "💨", name = "dash", keywords = "wind fast running" }
    , { unicode = "🕳️", name = "hole", keywords = "void" }
    , { unicode = "💣", name = "bomb", keywords = "boom explosive" }
    , { unicode = "💬", name = "speech-balloon", keywords = "comment talk chat" }
    , { unicode = "💭", name = "thought-balloon", keywords = "think bubble" }
    , { unicode = "🗯️", name = "right-anger", keywords = "shout" }
    , { unicode = "💤", name = "zzz", keywords = "sleep rest" }

    -- Nature & Animals
    , { unicode = "🐶", name = "dog", keywords = "puppy pet animal" }
    , { unicode = "🐱", name = "cat", keywords = "kitten pet animal" }
    , { unicode = "🐭", name = "mouse", keywords = "animal" }
    , { unicode = "🐹", name = "hamster", keywords = "pet animal" }
    , { unicode = "🐰", name = "rabbit", keywords = "bunny animal" }
    , { unicode = "🦊", name = "fox", keywords = "animal" }
    , { unicode = "🐻", name = "bear", keywords = "animal" }
    , { unicode = "🐼", name = "panda", keywords = "animal" }
    , { unicode = "🐨", name = "koala", keywords = "animal" }
    , { unicode = "🐯", name = "tiger", keywords = "animal" }
    , { unicode = "🦁", name = "lion", keywords = "animal king" }
    , { unicode = "🐮", name = "cow", keywords = "animal" }
    , { unicode = "🐷", name = "pig", keywords = "animal" }
    , { unicode = "🐸", name = "frog", keywords = "animal toad" }
    , { unicode = "🐵", name = "monkey-face", keywords = "animal" }
    , { unicode = "🐔", name = "chicken", keywords = "animal bird" }
    , { unicode = "🐧", name = "penguin", keywords = "animal bird" }
    , { unicode = "🐦", name = "bird", keywords = "animal fly" }
    , { unicode = "🦅", name = "eagle", keywords = "bird animal" }
    , { unicode = "🦉", name = "owl", keywords = "bird animal wise" }
    , { unicode = "🐝", name = "bee", keywords = "insect honey" }
    , { unicode = "🐛", name = "bug", keywords = "insect" }
    , { unicode = "🦋", name = "butterfly", keywords = "insect" }
    , { unicode = "🐌", name = "snail", keywords = "slow" }
    , { unicode = "🐙", name = "octopus", keywords = "animal sea" }
    , { unicode = "🐬", name = "dolphin", keywords = "animal sea" }
    , { unicode = "🐳", name = "whale", keywords = "animal sea" }
    , { unicode = "🦈", name = "shark", keywords = "animal sea fish" }
    , { unicode = "🐊", name = "crocodile", keywords = "animal" }
    , { unicode = "🐘", name = "elephant", keywords = "animal" }
    , { unicode = "🦒", name = "giraffe", keywords = "animal" }
    , { unicode = "🦘", name = "kangaroo", keywords = "animal" }
    , { unicode = "🐿️", name = "chipmunk", keywords = "animal squirrel" }
    , { unicode = "🌸", name = "cherry-blossom", keywords = "flower spring" }
    , { unicode = "🌹", name = "rose", keywords = "flower love" }
    , { unicode = "🌻", name = "sunflower", keywords = "flower" }
    , { unicode = "🌺", name = "hibiscus", keywords = "flower" }
    , { unicode = "🌷", name = "tulip", keywords = "flower" }
    , { unicode = "🌱", name = "seedling", keywords = "plant grow" }
    , { unicode = "🌲", name = "evergreen", keywords = "tree nature" }
    , { unicode = "🌳", name = "deciduous-tree", keywords = "nature" }
    , { unicode = "🌴", name = "palm-tree", keywords = "tropical" }
    , { unicode = "🌵", name = "cactus", keywords = "desert" }
    , { unicode = "🍀", name = "four-leaf-clover", keywords = "luck" }
    , { unicode = "🍁", name = "maple-leaf", keywords = "fall autumn canada" }
    , { unicode = "🍂", name = "fallen-leaf", keywords = "autumn" }
    , { unicode = "🍃", name = "leaf-wind", keywords = "nature blow" }

    -- Food & Drink
    , { unicode = "🍎", name = "apple", keywords = "fruit red" }
    , { unicode = "🍊", name = "orange", keywords = "fruit tangerine" }
    , { unicode = "🍋", name = "lemon", keywords = "fruit sour" }
    , { unicode = "🍌", name = "banana", keywords = "fruit" }
    , { unicode = "🍉", name = "watermelon", keywords = "fruit summer" }
    , { unicode = "🍇", name = "grapes", keywords = "fruit wine" }
    , { unicode = "🍓", name = "strawberry", keywords = "fruit berry" }
    , { unicode = "🍒", name = "cherries", keywords = "fruit" }
    , { unicode = "🍑", name = "peach", keywords = "fruit" }
    , { unicode = "🥑", name = "avocado", keywords = "fruit guacamole" }
    , { unicode = "🍕", name = "pizza", keywords = "food" }
    , { unicode = "🍔", name = "burger", keywords = "food hamburger" }
    , { unicode = "🍟", name = "fries", keywords = "food french" }
    , { unicode = "🌭", name = "hotdog", keywords = "food sausage" }
    , { unicode = "🌮", name = "taco", keywords = "food mexican" }
    , { unicode = "🌯", name = "burrito", keywords = "food wrap" }
    , { unicode = "🍿", name = "popcorn", keywords = "food movie" }
    , { unicode = "🧁", name = "cupcake", keywords = "food dessert" }
    , { unicode = "🍰", name = "cake", keywords = "food dessert birthday" }
    , { unicode = "🍩", name = "donut", keywords = "food dessert" }
    , { unicode = "🍪", name = "cookie", keywords = "food dessert" }
    , { unicode = "🍫", name = "chocolate", keywords = "food dessert candy" }
    , { unicode = "🍭", name = "lollipop", keywords = "food dessert candy" }
    , { unicode = "🍬", name = "candy", keywords = "food dessert sweet" }
    , { unicode = "☕", name = "coffee", keywords = "drink hot beverage" }
    , { unicode = "🍵", name = "tea", keywords = "drink hot beverage" }
    , { unicode = "🥤", name = "cup-straw", keywords = "drink soda" }
    , { unicode = "🍺", name = "beer", keywords = "drink alcohol" }
    , { unicode = "🍻", name = "beers", keywords = "drink alcohol cheers" }
    , { unicode = "🥂", name = "champagne-glasses", keywords = "drink toast celebrate" }
    , { unicode = "🍷", name = "wine", keywords = "drink alcohol" }
    , { unicode = "🥃", name = "tumbler-glass", keywords = "drink whiskey" }
    , { unicode = "🧃", name = "juice-box", keywords = "drink" }

    -- Activities & Sports
    , { unicode = "⚽", name = "soccer", keywords = "sport football ball" }
    , { unicode = "🏀", name = "basketball", keywords = "sport ball" }
    , { unicode = "🏈", name = "football", keywords = "sport american ball" }
    , { unicode = "⚾", name = "baseball", keywords = "sport ball" }
    , { unicode = "🎾", name = "tennis", keywords = "sport ball" }
    , { unicode = "🏐", name = "volleyball", keywords = "sport ball" }
    , { unicode = "🏓", name = "ping-pong", keywords = "sport table tennis" }
    , { unicode = "🎯", name = "dart", keywords = "target bullseye" }
    , { unicode = "🏆", name = "trophy", keywords = "win prize award" }
    , { unicode = "🥇", name = "gold-medal", keywords = "win first" }
    , { unicode = "🥈", name = "silver-medal", keywords = "second" }
    , { unicode = "🥉", name = "bronze-medal", keywords = "third" }
    , { unicode = "🎮", name = "video-game", keywords = "controller play" }
    , { unicode = "🎲", name = "dice", keywords = "game random" }
    , { unicode = "🧩", name = "puzzle", keywords = "jigsaw game" }
    , { unicode = "🎭", name = "performing-arts", keywords = "theater drama" }
    , { unicode = "🎨", name = "art", keywords = "paint palette" }
    , { unicode = "🎬", name = "clapper", keywords = "film movie" }
    , { unicode = "🎤", name = "microphone", keywords = "sing karaoke" }
    , { unicode = "🎧", name = "headphones", keywords = "music listen" }
    , { unicode = "🎵", name = "musical-note", keywords = "music sound" }
    , { unicode = "🎶", name = "notes", keywords = "music sound" }
    , { unicode = "🎸", name = "guitar", keywords = "music rock" }
    , { unicode = "🎹", name = "piano", keywords = "music keys" }
    , { unicode = "🎺", name = "trumpet", keywords = "music horn" }

    -- Travel & Places
    , { unicode = "🚗", name = "car", keywords = "vehicle drive" }
    , { unicode = "🚕", name = "taxi", keywords = "vehicle cab" }
    , { unicode = "🚌", name = "bus", keywords = "vehicle transit" }
    , { unicode = "🚀", name = "rocket", keywords = "space launch ship" }
    , { unicode = "✈️", name = "airplane", keywords = "travel fly" }
    , { unicode = "🚁", name = "helicopter", keywords = "fly vehicle" }
    , { unicode = "🚂", name = "locomotive", keywords = "train" }
    , { unicode = "🚢", name = "ship", keywords = "boat sea" }
    , { unicode = "🏠", name = "house", keywords = "home" }
    , { unicode = "🏢", name = "office", keywords = "building work" }
    , { unicode = "🏥", name = "hospital", keywords = "health doctor" }
    , { unicode = "🏫", name = "school", keywords = "education" }
    , { unicode = "⛪", name = "church", keywords = "religion" }
    , { unicode = "🗽", name = "statue-liberty", keywords = "new york" }
    , { unicode = "🗼", name = "tokyo-tower", keywords = "japan" }
    , { unicode = "🌍", name = "earth-africa", keywords = "globe world" }
    , { unicode = "🌎", name = "earth-americas", keywords = "globe world" }
    , { unicode = "🌏", name = "earth-asia", keywords = "globe world" }
    , { unicode = "🌙", name = "crescent-moon", keywords = "night" }
    , { unicode = "🌞", name = "sun-face", keywords = "bright day" }
    , { unicode = "⭐", name = "star", keywords = "night favorite" }
    , { unicode = "🌟", name = "glowing-star", keywords = "sparkle" }
    , { unicode = "⚡", name = "zap", keywords = "lightning electric thunder" }
    , { unicode = "🔥", name = "fire", keywords = "hot flame lit" }
    , { unicode = "🌈", name = "rainbow", keywords = "color spectrum" }
    , { unicode = "☀️", name = "sun", keywords = "bright day weather" }
    , { unicode = "🌤️", name = "sun-cloud", keywords = "weather" }
    , { unicode = "⛅", name = "partly-sunny", keywords = "weather cloud" }
    , { unicode = "🌧️", name = "rain", keywords = "weather water" }
    , { unicode = "⛈️", name = "thunderstorm", keywords = "weather rain" }
    , { unicode = "❄️", name = "snowflake", keywords = "winter cold" }
    , { unicode = "☃️", name = "snowman", keywords = "winter cold" }
    , { unicode = "🌊", name = "wave-water", keywords = "ocean sea surf" }

    -- Objects
    , { unicode = "⌚", name = "watch", keywords = "time clock" }
    , { unicode = "📱", name = "phone", keywords = "mobile cell" }
    , { unicode = "💻", name = "laptop", keywords = "computer pc" }
    , { unicode = "🖥️", name = "desktop", keywords = "computer monitor" }
    , { unicode = "⌨️", name = "keyboard", keywords = "type computer" }
    , { unicode = "🖱️", name = "mouse-computer", keywords = "click" }
    , { unicode = "💾", name = "floppy-disk", keywords = "save" }
    , { unicode = "💿", name = "cd", keywords = "disc music" }
    , { unicode = "📷", name = "camera", keywords = "photo picture" }
    , { unicode = "📹", name = "video-camera", keywords = "film record" }
    , { unicode = "🔍", name = "magnifying-glass", keywords = "search zoom" }
    , { unicode = "🔑", name = "key", keywords = "lock password" }
    , { unicode = "🔒", name = "lock", keywords = "security private" }
    , { unicode = "🔓", name = "unlock", keywords = "security open" }
    , { unicode = "🔔", name = "bell", keywords = "notification alert ring" }
    , { unicode = "🔕", name = "no-bell", keywords = "mute silent" }
    , { unicode = "📢", name = "loudspeaker", keywords = "announce volume" }
    , { unicode = "📣", name = "megaphone", keywords = "announce cheer" }
    , { unicode = "📌", name = "pushpin", keywords = "pin location" }
    , { unicode = "📎", name = "paperclip", keywords = "attach" }
    , { unicode = "📏", name = "ruler", keywords = "measure" }
    , { unicode = "✏️", name = "pencil", keywords = "write edit" }
    , { unicode = "🖊️", name = "pen", keywords = "write" }
    , { unicode = "📝", name = "memo", keywords = "note write document" }
    , { unicode = "📁", name = "folder", keywords = "file directory" }
    , { unicode = "📂", name = "open-folder", keywords = "file directory" }
    , { unicode = "📅", name = "calendar", keywords = "date schedule" }
    , { unicode = "📆", name = "tear-off-calendar", keywords = "date schedule" }
    , { unicode = "📊", name = "bar-chart", keywords = "graph stats" }
    , { unicode = "📈", name = "chart-up", keywords = "graph growth trend" }
    , { unicode = "📉", name = "chart-down", keywords = "graph decline trend" }
    , { unicode = "📋", name = "clipboard", keywords = "list copy" }
    , { unicode = "📖", name = "book", keywords = "read open" }
    , { unicode = "📚", name = "books", keywords = "read library" }
    , { unicode = "🔗", name = "link", keywords = "chain url" }
    , { unicode = "💡", name = "bulb", keywords = "idea light" }
    , { unicode = "🔧", name = "wrench", keywords = "tool fix" }
    , { unicode = "🔨", name = "hammer", keywords = "tool build" }
    , { unicode = "⚙️", name = "gear", keywords = "settings cog" }
    , { unicode = "🧲", name = "magnet", keywords = "attract" }
    , { unicode = "🧪", name = "test-tube", keywords = "science experiment" }
    , { unicode = "🧬", name = "dna", keywords = "science genetics" }
    , { unicode = "💊", name = "pill", keywords = "medicine health" }
    , { unicode = "🩹", name = "bandaid", keywords = "heal fix" }
    , { unicode = "🏷️", name = "label", keywords = "tag price" }
    , { unicode = "🎁", name = "gift", keywords = "present birthday" }
    , { unicode = "🎈", name = "balloon", keywords = "party celebrate" }
    , { unicode = "🎉", name = "tada", keywords = "party celebrate congratulations" }
    , { unicode = "🎊", name = "confetti", keywords = "party celebrate" }
    , { unicode = "🎗️", name = "ribbon", keywords = "awareness" }
    , { unicode = "🏅", name = "medal", keywords = "award sports" }

    -- Symbols & Signs
    , { unicode = "✅", name = "check", keywords = "yes done complete" }
    , { unicode = "❌", name = "x", keywords = "no wrong delete cross" }
    , { unicode = "❓", name = "question", keywords = "help what" }
    , { unicode = "❗", name = "exclamation", keywords = "important alert" }
    , { unicode = "‼️", name = "double-exclamation", keywords = "important alert" }
    , { unicode = "⁉️", name = "exclamation-question", keywords = "surprise" }
    , { unicode = "⚠️", name = "warning", keywords = "caution alert danger" }
    , { unicode = "🚫", name = "no-entry", keywords = "forbidden prohibited" }
    , { unicode = "🔴", name = "red-circle", keywords = "dot" }
    , { unicode = "🟠", name = "orange-circle", keywords = "dot" }
    , { unicode = "🟡", name = "yellow-circle", keywords = "dot" }
    , { unicode = "🟢", name = "green-circle", keywords = "dot" }
    , { unicode = "🔵", name = "blue-circle", keywords = "dot" }
    , { unicode = "🟣", name = "purple-circle", keywords = "dot" }
    , { unicode = "⚪", name = "white-circle", keywords = "dot" }
    , { unicode = "⚫", name = "black-circle", keywords = "dot" }
    , { unicode = "🔶", name = "large-orange-diamond", keywords = "shape" }
    , { unicode = "🔷", name = "large-blue-diamond", keywords = "shape" }
    , { unicode = "▶️", name = "play", keywords = "start forward" }
    , { unicode = "⏸️", name = "pause", keywords = "stop wait" }
    , { unicode = "⏹️", name = "stop-button", keywords = "square end" }
    , { unicode = "⏩", name = "fast-forward", keywords = "skip" }
    , { unicode = "⏪", name = "rewind", keywords = "back" }
    , { unicode = "🔀", name = "shuffle", keywords = "random" }
    , { unicode = "🔁", name = "repeat", keywords = "loop" }
    , { unicode = "♻️", name = "recycle", keywords = "environment green" }
    , { unicode = "✨", name = "sparkles", keywords = "shine glitter new" }
    , { unicode = "🏳️", name = "white-flag", keywords = "surrender" }
    , { unicode = "🏴", name = "black-flag", keywords = "" }
    , { unicode = "🚩", name = "red-flag", keywords = "warning triangular" }

    -- Misc popular
    , { unicode = "🎃", name = "jack-o-lantern", keywords = "halloween pumpkin" }
    , { unicode = "🎄", name = "christmas-tree", keywords = "holiday xmas" }
    , { unicode = "🎅", name = "santa", keywords = "christmas" }
    , { unicode = "🧑‍💻", name = "technologist", keywords = "developer coder programmer" }
    , { unicode = "👨‍💻", name = "man-technologist", keywords = "developer coder" }
    , { unicode = "👩‍💻", name = "woman-technologist", keywords = "developer coder" }
    , { unicode = "🧑‍🔬", name = "scientist", keywords = "research" }
    , { unicode = "👨‍🎓", name = "man-student", keywords = "graduate school" }
    , { unicode = "👩‍🎓", name = "woman-student", keywords = "graduate school" }
    , { unicode = "🧑‍🏫", name = "teacher", keywords = "education" }
    , { unicode = "🦸", name = "superhero", keywords = "hero" }
    , { unicode = "🦹", name = "supervillain", keywords = "villain evil" }
    , { unicode = "💪🏽", name = "muscle-medium", keywords = "strong flex" }
    , { unicode = "🤷", name = "shrug", keywords = "idk whatever dunno" }
    , { unicode = "🤦", name = "facepalm", keywords = "disappointed" }
    , { unicode = "🙋", name = "raising-hand", keywords = "hi volunteer" }
    , { unicode = "🙇", name = "bowing", keywords = "sorry respect" }
    , { unicode = "💃", name = "dancer", keywords = "salsa" }
    , { unicode = "🕺", name = "man-dancing", keywords = "disco" }
    , { unicode = "🧘", name = "person-meditation", keywords = "yoga zen calm" }
    , { unicode = "🛒", name = "shopping-cart", keywords = "buy store" }
    , { unicode = "🎓", name = "graduation-cap", keywords = "school education" }
    , { unicode = "💍", name = "ring", keywords = "wedding engaged diamond" }
    , { unicode = "👑", name = "crown", keywords = "king queen royal" }
    , { unicode = "🎩", name = "top-hat", keywords = "formal classy" }
    , { unicode = "🧢", name = "billed-cap", keywords = "hat baseball" }
    , { unicode = "👓", name = "glasses", keywords = "eyes sight" }
    , { unicode = "🕶️", name = "dark-sunglasses", keywords = "cool shades" }
    , { unicode = "🌐", name = "globe-meridians", keywords = "world internet web" }
    , { unicode = "📧", name = "email", keywords = "mail letter" }
    , { unicode = "📮", name = "postbox", keywords = "mail" }
    , { unicode = "🗑️", name = "wastebasket", keywords = "trash delete garbage" }
    , { unicode = "📦", name = "package", keywords = "box delivery" }
    , { unicode = "🔐", name = "locked-key", keywords = "security" }
    , { unicode = "🛡️", name = "shield", keywords = "protect security" }
    , { unicode = "⏰", name = "alarm-clock", keywords = "time wake" }
    , { unicode = "⏳", name = "hourglass", keywords = "time wait" }
    , { unicode = "🔮", name = "crystal-ball", keywords = "fortune magic" }
    , { unicode = "🧭", name = "compass", keywords = "navigation direction" }
    , { unicode = "🪙", name = "coin", keywords = "money gold" }
    , { unicode = "💰", name = "money-bag", keywords = "dollar rich" }
    , { unicode = "💵", name = "dollar", keywords = "money cash" }
    , { unicode = "🏦", name = "bank", keywords = "money finance" }
    , { unicode = "📐", name = "triangular-ruler", keywords = "math geometry" }
    , { unicode = "🧮", name = "abacus", keywords = "math calculate" }
    ]
