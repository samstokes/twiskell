module Sentiment where
import Control.Applicative (liftA)
import Data.List (intersect)
import Data.Maybe (fromMaybe)
import Twitter.Types

data PosNeg = Positive | Negative

keywords =
		[ ("rocks", (0.2, 0.0))
		, ("sucks", (0.0, 0.2))
		, ("great", (0.2, 0.0))
		, ("awesome", (0.2, 0.0))
		, ("crap", (0.0, 0.2))
		, (":)", (0.1, 0.0))
		, (":(", (0.0, 0.1)) ]


prob :: PosNeg -> String -> Float
prob posneg text =
		product $ map (probPosNegGivenKeyword posneg) foundKeywords
		where
			foundKeywords = intersect (words text) (map fst keywords)


probPosNegGivenKeyword posneg keyword =
		probKeywordGivenPosNeg keyword posneg * probPosNeg posneg / probKeyword keyword

probKeywordGivenPosNeg keyword Positive = fromMaybe 0.0 $ liftA fst $ lookup keyword keywords
probKeywordGivenPosNeg keyword Negative = fromMaybe 0.0 $ liftA snd $ lookup keyword keywords

probPosNeg _ = 0.5

probKeyword _ = 0.1

-- P(h|d) = P(d|h) * P(h)
--          -------------
--              P(d)

-- P(d) = P(Keyword) = relative word frequency in corpus (global timeline?) (trends?)
-- P(h) = P(PosNeg) = relative pos/neg frequency (hand-trained) in corpus (try 0.5 as first hack?)
-- P(d|h) = relative word frequency in pos/neg tweets
