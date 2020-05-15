{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeApplications          #-}
module Render where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Planner
import Planner.Config
import Planner.CSpace as CS
import qualified Data.Map as Map

type Dia = Diagram B

renderGrid :: (Monad m) => PlannerT m Dia
renderGrid = do
    g <- Map.toList <$> (grid <$> getCSpace)
    resolution <- gridResolution <$> getConfig
    let dias = map (\((ix, iy, _), _) -> tosquare resolution (ix, iy)) g
    pure $ mconcat dias

    where tosquare :: Double -> (Int, Int) -> QDiagram B V2 Double Any 
          tosquare res (ix, iy) = let ix' = (fromIntegral ix) :: Double
                                      iy' = (fromIntegral iy) :: Double
                                   in rect res (-res) # translateX (ix'*res)
                                                    # translateY (iy'*res)

renderVehicle :: (Monad m) => Double -> PlannerT m Dia
renderVehicle a = do
    wheelBase <- wheelBase <$> getConfig
    let length = wheelBase
        width = wheelBase / 2
        vehicle = rect length width # translateX (length/2)
                                    # showOrigin
        wheelLength = length/5
        wheelWidth = length/10
        wheel = rect wheelLength wheelWidth
        lFrontWheel = wheel # rotate (a @@ rad)
                            # translate (V2 length (width/2))
        rFrontWheel = wheel # rotate (a @@ rad)
                            # translate (V2 length (-width/2))
        lBackWheel = wheel # translate (V2 0 (width/2))
        rBackWheel = wheel # translate (V2 0 (-width/2))
        frontWheels = (lFrontWheel <> rFrontWheel) 
        backWheels = (lBackWheel <> rBackWheel)
    return $ vehicle <> frontWheels <> backWheels

renderPose :: (Monad m) => (Double, Double, Double) -> Double -> PlannerT m Dia
renderPose (x,y, o) a = do
    v <- renderVehicle a
    let v' = v # rotate (o @@ rad)
               # translate (V2 x y)
    return v'
pose :: (Monad m) => [Node] -> PlannerT m [Dia]
pose ((Node _ p1 _) : (Node _ p2 a)  : [])    = do
    p' <- renderPose p1 a
    p''<- renderPose p2 0
    return [p', p'']
pose ((Node _ p1 _) : (Node i2 p2 a) : poses) = do
    p' <- renderPose p1 a
    rest <- pose ((Node i2 p2 a) : poses)
    return $ p' : rest 

renderPath :: (Monad m) => [Node] -> PlannerT m Dia
renderPath poses = do
    dias <- pose poses
    return $ mconcat dias