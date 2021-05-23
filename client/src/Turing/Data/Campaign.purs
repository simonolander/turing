module Turing.Data.Campaign where

import Prelude

import Turing.Data.Spec (Spec, SpecGoal(..))

type CampaignId = String

type Campaign =
    { id :: CampaignId
    , name :: String
    , specs :: Array Spec
    }

campaigns :: Array Campaign
campaigns =
    [ { id: "jJe78fb"
      , name: "Busy beavers"
      , specs:
          [ { id: "U0481dB"
            , name: "BB1"
            , maxNumberOfCards: 1
            , goal: BusyBeaver
            }
          , { id: "0Y1R2JQ"
            , name: "BB2"
            , maxNumberOfCards: 2
            , goal: BusyBeaver
            }
          , { id: "TNntfDM"
            , name: "BB3"
            , maxNumberOfCards: 3
            , goal: BusyBeaver
            }
          , { id: "n6CwGpA"
            , name: "BB4"
            , maxNumberOfCards: 4
            , goal: BusyBeaver
            }
          , { id: "TOrmdjW"
            , name: "BB5"
            , maxNumberOfCards: 5
            , goal: BusyBeaver
            }
          ]
      }
    ]
