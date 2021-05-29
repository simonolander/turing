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
          , { id: "YWbLhkC"
            , name: "BB5"
            , maxNumberOfCards: 5
            , goal: BusyBeaver
            }
          , { id: "cR11nFB"
            , name: "BB6"
            , maxNumberOfCards: 6
            , goal: BusyBeaver
            }
          , { id: "SZKns9S"
            , name: "BB7"
            , maxNumberOfCards: 7
            , goal: BusyBeaver
            }
          , { id: "2YWLM6b"
            , name: "BB8"
            , maxNumberOfCards: 8
            , goal: BusyBeaver
            }
          , { id: "M1GREyY"
            , name: "BB9"
            , maxNumberOfCards: 9
            , goal: BusyBeaver
            }
          , { id: "65g53e3"
            , name: "BB10"
            , maxNumberOfCards: 10
            , goal: BusyBeaver
            }
          , { id: "XQnXYnS"
            , name: "BB11"
            , maxNumberOfCards: 11
            , goal: BusyBeaver
            }
          ]
      }
    ]
