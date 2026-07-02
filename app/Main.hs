import System.Environment (
                           getArgs
                          )
import System.IO          (
                           Handle,
                           openFile,
                           hPutStrLn,
                           hClose,
                           IOMode (WriteMode)
                          )
import Control.Monad      (
                           when
                          )
import Text.Printf        (printf)
import Linear.V3
import Linear.Quaternion

import Utils              (
                           displayUsage,
                           write_to_file,
                           parseArgs
                          )
import MathUtils          (
                           compute_V3_magnitude,
                           compute_V3_length,
                           createRotationQuaternion,
                           vector_to_quaternion,
                           compute_quaternion_length,
                           compute_quaternion_magnitude,
                           convert_to_unit_quaternion,
                           preMultiplyVector,
                           computeQuaternionConjugate,
                           displayDataVector,
                           displayDataQuaternion
                          )


main :: IO ()
main = do

    args <- getArgs

    if length (args) /= 8
    then do

        displayUsage

    else do

        case parseArgs args of

            Just (verbose,
                  vector_x,
                  vector_y,
                  vector_z,
                  angle_rotation,
                  vector_axis_x,
                  vector_axis_y,
                  vector_axis_z) -> do

                let

                    -- Vector to be rotated.

                    vector            = V3 vector_x vector_y vector_z
                    quaternion_vector = vector_to_quaternion (vector)

                    -- Rotation quaternion and its conjugate.

                    quaternion_rotation           = createRotationQuaternion   angle_rotation vector_axis_x vector_axis_y vector_axis_z
                    quaternion_rotation_conjugate = computeQuaternionConjugate quaternion_rotation

                    -- Pre-multiply the rotation quaternion with the vector.

                    quaternion_pre_multiply = quaternion_rotation * quaternion_vector

                    -- Fully rotated quaternion.

                    quaternion_rotated = quaternion_pre_multiply * quaternion_rotation_conjugate

                    Quaternion w (V3 x y z) = quaternion_rotated

                when verbose $ do

                    displayDataVector     vector                  "Vector to be rotated"
                    displayDataQuaternion quaternion_rotation     "Rotation quaternion"
                    displayDataQuaternion quaternion_pre_multiply "Quaternion pre-multiply"
                    displayDataQuaternion quaternion_rotated      "Quaternion rotated"

                printf "%0.3f, %0.3f, %0.3f, %0.3f\n" w x y z

                -- Open a file to write data into.

                -- handle_file <- openFile "output.txt" WriteMode

                -- Call a function which will write data into this file.

                -- write_to_file handle_file "Hello, Haskel!"

                -- Close this file.

                -- hClose handle_file

                -- Pre-multiply the quaternion with the vector.

            Nothing -> do

                putStrLn "Error: invalid or missing vector arguments"
