module
Utils
(
 someFunc,
 displayUsage,
 parseArgs,
 write_to_file
)
where

import Text.Printf (printf)
import System.IO   (Handle,
                    hPutStrLn)
import Text.Read   (readMaybe)


-- A simple greeting function

someFunc :: IO ()
someFunc = putStrLn "Hello from my custom Haskell library!"


displayUsage :: IO ()
displayUsage = do

    printf "\n"
    printf "Rotate a 3d vector using a quaternion.\n"
    printf "\n"
    printf "Usage:\n"
    printf "\n"
    printf "  vectorQuaternionRotate verbose vector_x vector_y vector_z rotationAngle axis_x axis_y axis_z\n"
    printf "\n"
    printf "where:\n"
    printf "\n"
    printf "  verbose       : utility runs in a verbose fashion [Haskell type Bool : True | False]\n"
    printf "  vector_x      : the x component of the 3d vector that is to be rotated [Haskell type Double]\n"
    printf "  vector_y      : the y component of the 3d vector that is to be rotated [Haskell type Double]\n"
    printf "  vector_z      : the z component of the 3d vector that is to be rotated [Haskell type Double]\n"
    printf "  rotationAngle : the angle by which the 3d vector is to be rotated [Haskell type Double]\n"
    printf "  axis_x        : the x component of the 3d vector which acts as the axis of rotation [Haskell type Double]\n"
    printf "  axis_y        : the y component of the 3d vector which acts as the axis of rotation [Haskell type Double]\n"
    printf "  axis_z        : the z component of the 3d vector which acts as the axis of rotation [Haskell type Double]\n"
    printf "\n"

parseArgs :: [String] -> Maybe (Bool, Double, Double, Double, Double, Double, Double, Double)
parseArgs args = do

    verbose        <- readMaybe (args !! 0)

    vector_x       <- readMaybe (args !! 1)
    vector_y       <- readMaybe (args !! 2)
    vector_z       <- readMaybe (args !! 3)

    angle_rotation <- readMaybe (args !! 4)

    vector_axis_x  <- readMaybe (args !! 5)
    vector_axis_y  <- readMaybe (args !! 6)
    vector_axis_z  <- readMaybe (args !! 7)

    return (verbose, vector_x, vector_y, vector_z, angle_rotation, vector_axis_x, vector_axis_y, vector_axis_z)


write_to_file :: Handle ->
                 String ->
                 IO ()
write_to_file    handle_file
                 data_to_write = do

    hPutStrLn handle_file data_to_write
