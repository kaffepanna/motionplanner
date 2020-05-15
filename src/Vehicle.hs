module Vehicle where

import Data.Fixed (mod')


nextPose :: (Double, Double, Double)       -- ^ Current position
             -> Double                         -- ^ Distance to travel
             -> Double                         -- ^ wheelbase length
             -> Double                         -- ^ Steering angle
             -> (Double, Double, Double) --- ^ Resulting position
nextPose (x, y, o) d l a =
    let b = d / l * tan a -- turning angle (dimension less?)
        ob = o + b -- treet b as an angle?
        o' =  ob `mod'` (2 * pi) 
     in if abs b > 0.001 then -- for small b's simplify
            let 
                r = d / b
                cx = x - sin o * r
                cy = y + cos o * r
                x' = cx + sin ob * r
                y' = cy - cos ob * r
            in (x', y', ob)
    else
        let x' = x + d * cos o
            y' = y + d * sin o
         in (x', y', o')