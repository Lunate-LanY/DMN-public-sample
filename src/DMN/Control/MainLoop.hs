module DMN.Control.MainLoop(
    mainloop,
    mainloop_ls
)  where

import FRP.Yampa
import Data.IORef
import System.Random 
import System.IO
import qualified Data.Vector as V

import DMN.Control.MainSF
import DMN.Utils.Structure
--
mainloop :: IO ()
mainloop = do
    timeRef <- newIORef 0.0
    handle_fire         <- openFile "./result/results.txt" AppendMode
    handle_gliaMP       <- openFile "./result/gliaMP.txt" AppendMode
    handle_ambient_gaba <- openFile "./result/ambientGABA.txt" AppendMode
    handle_P_potential  <- openFile "./result/P_potential.txt" AppendMode
    let 
        initialize :: IO (StdGen, Double)
        initialize = do
            stdGen <- getStdGen
            return (stdGen, 0.0)

        newSample :: Bool -> IO (DTime, Maybe (StdGen, Double))
        newSample _ = do
            t <- readIORef timeRef
            stdGen_ <- newStdGen
            return (0.0001, Just (stdGen_, t))

        outputProcessing :: Bool -> (V.Vector Double, V.Vector (Event Int)) -> IO Bool
        outputProcessing _ (resVect, fireVect) = do
            let 
                printFireVect :: V.Vector (Event Int) -> IO ()
                printFireVect vect = do
                    hPutStr handle_fire $ init $ tail $ show $ convert_Event2Bool2Str <$> (V.slice uP_start_pos 160 vect)
                    hPutStr handle_fire ","
                    hPutStr handle_fire $ init $ tail $ show $ convert_Event2Bool2Str <$> (V.slice uP_DMN_start_pos 20 vect)
                      where
                        -- Caution: Event only implies the cell is in the refractory period, but only when it is in the absolute
                        --          refractory period (number in the Event >= 10) can it be called a firing cell
                        convert_Event2Bool2Str :: Event Int -> Int
                        convert_Event2Bool2Str p = if ((isEvent p) && (fromEvent p >= 10)) then 1 else 0
                printResVect :: V.Vector Double -> IO ()
                printResVect vect = do
                    hPutStr handle_ambient_gaba $ init $ tail $ show $ V.slice gaba_ext_start_pos 160 vect
                    hPutStr handle_gliaMP $ init $ tail $ show $ V.slice uGl_start_pos 160 vect
                    -- saving of P potential
                    hPutStr handle_P_potential $ init $ tail $ show $ V.slice uP_start_pos 160 vect  -- membrane potential of P in Nsen
                    hPutStr handle_P_potential ","
                    hPutStr handle_P_potential $ init $ tail $ show $ V.slice uP_DMN_start_pos 20 vect -- membrane potential of P in DMN
            printFireVect fireVect
            printResVect resVect
            hPutStr handle_fire         "\n"
            hPutStr handle_ambient_gaba "\n"
            hPutStr handle_gliaMP       "\n"
            hPutStr handle_P_potential  "\n"
            time_before <- readIORef timeRef              -- when one step was completed, actual time steps forward (t + 1), 
                                                          -- while timeRef stays the same (remains t)
            let time_now = time_before + 0.0001
            writeIORef timeRef (time_now)
            if (time_now >= 3.5) then (return True) else (return False)  -- if time_now >= 3.5s, stop the simulation,
                                                                         -- else continue the simulation

    reactimate initialize newSample outputProcessing mainSF
    hClose handle_fire
    hClose handle_gliaMP
    hClose handle_ambient_gaba
    hClose handle_P_potential

mainloop_ls :: IO ()
mainloop_ls = do
    timeRef <- newIORef 0.0
    handle_fire <- openFile "./result/results.txt" AppendMode
    let 
        initialize :: IO (StdGen, Double)
        initialize = do
            stdGen <- getStdGen
            return (stdGen, 0.0)

        newSample :: Bool -> IO (DTime, Maybe (StdGen, Double))
        newSample _ = do
            t <- readIORef timeRef
            stdGen_ <- newStdGen
            return (0.0001, Just (stdGen_, t))

        outputProcessing :: Bool -> (V.Vector Double, V.Vector (Event Int)) -> IO Bool
        outputProcessing _ (resVect, fireVect) = do
            let 
                printFireVect :: V.Vector (Event Int) -> IO ()
                printFireVect vect = do
                    hPutStr handle_fire $ init $ tail $ show $ convert_Event2Bool2Str <$> (V.slice uP_start_pos 160 vect)
                    hPutStr handle_fire ","
                    hPutStr handle_fire $ init $ tail $ show $ convert_Event2Bool2Str <$> (V.slice uP_DMN_start_pos 20 vect)
                      where
                        -- Caution: Event only implies the cell is in the refractory period, but only when it is in the absolute
                        --          refractory period (number in the Event >= 10) can it be called a firing cell
                        convert_Event2Bool2Str :: Event Int -> Int
                        convert_Event2Bool2Str p = if ((isEvent p) && (fromEvent p >= 10)) then 1 else 0
            printFireVect fireVect
            hPutStr handle_fire         "\n"
            time_before <- readIORef timeRef              -- when one step was completed, actual time steps forward (t + 1), 
                                                          -- while timeRef stays the same (remains t)
            let time_now = time_before + 0.0001
            writeIORef timeRef (time_now)
            if (time_now >= 3.5) then (return True) else (return False)  -- if time_now >= 3.5s, stop the simulation,
                                                                         -- else continue the simulation

    reactimate initialize newSample outputProcessing mainSF
    hClose handle_fire