EXPORT tae := MODULE

	EXPORT lTae := RECORD
    INTEGER field1;
    INTEGER field2;
    INTEGER field3;
    INTEGER field4;
    INTEGER field5;
    INTEGER field6;
		INTEGER field7;
END;

	EXPORT dTae := DATASET('~::tae.csv', lTae AND NOT [field7], csv);


END;