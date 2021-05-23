module Turing.Data.Campaign where

import Prelude
import Turing.Data.Spec (Spec)

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
      , specs: []
      }
    ]
