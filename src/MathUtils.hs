module
MathUtils
(
 safeReadDouble,
 compute_V3_magnitude,
 compute_V3_length,
 vector_to_quaternion,
 compute_quaternion_magnitude,
 compute_quaternion_length,
 convert_to_unit_quaternion,
 preMultiplyVector,
 createRotationQuaternion,
 computeQuaternionConjugate,
 displayDataVector,
 displayDataQuaternion
)
where

import Text.Printf   (printf)
import System.IO     (Handle,
                      openFile,
                      hPutStrLn,
                      hClose,
                      IOMode (WriteMode))
import Text.Read     (readMaybe)
import Linear.Metric (norm,
                      normalize)
import Linear.Vector
import Linear.V3
import Linear.Quaternion


safeReadDouble :: String -> Maybe Double
safeReadDouble = readMaybe


compute_V3_magnitude :: V3 Double ->
                        Double
compute_V3_magnitude    vector =

    norm vector


compute_V3_length :: V3 Double ->
                     Double
compute_V3_length    vector =

    let

        V3 x y z = vector

    in

        sqrt ((x ^ 2) + (y ^ 2) + (z ^ 2))


compute_quaternion_magnitude :: Quaternion Double ->
                                Double
compute_quaternion_magnitude    quaternion =

    norm quaternion


convert_to_unit_quaternion :: Quaternion Double ->
                              Quaternion Double
convert_to_unit_quaternion    quaternion =

    normalize quaternion


vector_to_quaternion :: V3 Double ->
                        Quaternion Double
vector_to_quaternion    vector =

    Quaternion 0 vector


compute_quaternion_length :: Quaternion Double ->
                             Double
compute_quaternion_length    quaternion =

    let

        Quaternion w (V3 x y z) = quaternion

    in

        sqrt ((w ^ 2) + (x ^ 2) + (y ^ 2) + (z ^ 2))


computeQuaternionConjugate :: Quaternion Double ->
                              Quaternion Double
computeQuaternionConjugate    quaternion =

    let

        Quaternion w (V3 x y z) = quaternion

    in

        Quaternion w (V3 (-x) (-y) (-z))

createRotationQuaternion :: Double ->
                            Double ->
                            Double ->
                            Double ->
                            Quaternion Double
createRotationQuaternion    angle_rotation
                            vector_axis_x
                            vector_axis_y
                            vector_axis_z =

    -- printf "----------------------------------------\n"
    -- printf "angle_rotation = %f\n" angle_rotation
    -- printf "(vector_axis_x, vector_axis_y, vector_axis_z) = (%f, %f, %f)\n" vector_axis_x vector_axis_y vector_axis_z

    let

        -- The vector which forms the axis of rotation.
        --
        -- Create a unit vector from it.

        vector_axis         = V3 vector_axis_x vector_axis_y vector_axis_z
        vector_axis_unit    = normalize vector_axis
        vector_rotation     = sin (angle_rotation / 2.0) *^ vector_axis_unit

        angle_rotation_half = cos (angle_rotation / 2.0)

        quaternion_rotation = Quaternion angle_rotation_half vector_rotation

    in

        quaternion_rotation


preMultiplyVector :: V3 Double ->
                     Quaternion Double ->
                     IO (Quaternion Double)
preMultiplyVector    vector
                     quaternion_rotation = do

    let

        quaternion_vector = vector_to_quaternion vector

        V3 vector_x vector_y vector_z = vector
        Quaternion quaternion_rotation_scalar (V3 quaternion_rotation_x quaternion_rotation_y quaternion_rotation_z) = quaternion_rotation

        magnitude_quaternion_rotation = compute_quaternion_magnitude (quaternion_rotation)
        length_quaternion_rotation    = compute_quaternion_length (quaternion_rotation)


    printf "----------------------------------------\n"
    printf "vector               = <%0.6f, %0.6f, %0.6f>\n" vector_x vector_y vector_z
    printf "----------------------------------------\n"
    printf "quaternion_rotation  = <%0.6f, %0.6f, %0.6f, %0.6f>\n" quaternion_rotation_scalar quaternion_rotation_x quaternion_rotation_y quaternion_rotation_z

    -- This is not a pure function, so we need to use return.

    return (quaternion_rotation * quaternion_vector)


displayDataVector :: V3 Double ->
                     String ->
                     IO ()
displayDataVector    vector
                     dataDescription = do

    let

        V3 x y z         = vector
        magnitude_vector = compute_V3_magnitude vector
        length_vector    = compute_V3_length    vector

    printf "------------------------------------------------------------\n"
    printf "Data : %s\n" dataDescription
    printf "============================================================\n"
    printf "\n"
    printf "Value     = <%0.6f, %0.6f, %0.6f>\n" x y z
    printf "Magnitude = %f\n" magnitude_vector
    printf "Length    = %f\n" length_vector
    printf "------------------------------------------------------------\n"


displayDataQuaternion :: Quaternion Double ->
                         String ->
                         IO ()
displayDataQuaternion    quaternion
                         dataDescription = do

    let

        Quaternion w (V3 x y z) = quaternion
        magnitude_quaternion    = compute_quaternion_magnitude quaternion
        length_quaternion       = compute_quaternion_length    quaternion

    printf "------------------------------------------------------------\n"
    printf "Data : %s\n" dataDescription
    printf "============================================================\n"
    printf "\n"
    printf "Value     = <%0.6f, %0.6f, %0.6f, %0.6f>\n" w x y z
    printf "Magnitude = %f\n" magnitude_quaternion
    printf "Length    = %f\n" length_quaternion
    printf "------------------------------------------------------------\n"