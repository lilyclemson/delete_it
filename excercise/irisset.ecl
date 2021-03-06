﻿EXPORT irisset := MODULE
lData :=RECORD
    UNSIGNED id;
    REAL SepalLengthCm;
    REAL SepalWidthCm;
    REAL PetalLengthCm;
    REAL PetalWidthCm;
    REAL Species;
END;

//dataTemp := DATASET('~::iris.csv' ,lData,  CSV(HEADING(1)));
dataTemp := DATASET('~yy::iris::iris.csv',lData,  CSV(HEADING(1)));
EXPORT input:= TABLE(dataTemp,{id,SepalLengthCm, SepalWidthCm, PetalLengthCm,PetalWidthCm});

//OUTPUT(dataTemp);

END;



