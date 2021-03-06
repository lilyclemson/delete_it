﻿// IMPORT * FROM $;
// IMPORT Std.Str AS Str;
IMPORT TS;
IMPORT ML.Mat;
IMPORT ML;
IMPORT ML.Cluster.DF as DF;
IMPORT ML.Types AS Types;
IMPORT ML.Utils;
IMPORT ML.MAT AS Mat;

EXPORT YinyangKMeans(DATASET(Types.NumericField) d01,DATASET(Types.NumericField) d02,UNSIGNED n=1,REAL nConverge=0.0,DF.Default fDist=DF.Euclidean):=MODULE

SHARED lIterations:=RECORD 
TYPEOF(Types.NumericField.id) id;
TYPEOF(Types.NumericField.number) number;
SET OF TYPEOF(Types.NumericField.value) values;
END;

//make sure all ids are different
iOffset:=IF(MAX(d01,id)>MIN(d02,id),MAX(d01,id),0);

//transfrom the d02 to internal data structure
d02Prep:=PROJECT(d02,TRANSFORM(lIterations,SELF.id:=LEFT.id+iOffset;SELF.values:=[LEFT.value];SELF:=LEFT;));

// d02Prep; 

dCentroid0 := PROJECT(d02Prep,TRANSFORM(Types.NumericField,SELF.value:=LEFT.values[1];SELF:=LEFT;));
//OUTPUT(dCentroid0,NAMED('Initial_Centroids')); 

// get Gt 
//running Kmeans on dCentroid
//inputs

K := COUNT(d02)/2;
//K; 
//t:= IF(K/10<1, 1, K/10);
t:=2;
//t; 


//temporary solution to get dt is that dt = the first t cendroids of dCentroid 
temp := t * 2;
tempDt := dCentroid0[1..temp];
groupDs:=PROJECT(tempDt,TRANSFORM(Types.NumericField,SELF.id:=LEFT.id + k;SELF:=LEFT;));

/*
//transform ids of the centroids of dt starting from 1....
Types.NumericField transDt(Types.NumericField L, INTEGER c) := TRANSFORM
SELF.id := ROUNDUP(c/2);
SELF := L;
END;
dt := project(tempDt, transDt(LEFT, COUNTER));
// dt;
*/
nt := n;
tConverge := nConverge;

//run Kmeans on dCentroid to get Gt
//KmeansDt :=ML.Cluster.KMeans(dCentroid0,dt,nt,tConverge);
//********************
//KmeansDt :=ML.Cluster.KMeans(dCentroid0,tempdt,nt,tConverge);
KmeansDt :=ML.Cluster.KMeans(dCentroid0,groupDs,nt,tConverge);

//KmeansDt :=ML.Cluster.KMeans(dCentroid0,dCentroidGroup,nt,tConverge);
//KmeansDt :=ML.Cluster.KMeans(dCentroid0,dCentroidGroup,nt,tConverge);

Gt := TABLE(KmeansDt.Allegiances(), {x,y},y,x);//the assignment of each centroid to a group
//OUTPUT(Gt, NAMED('Gt'));

//initialize ub and lb for each data point
//get ub0
dDistances0 := ML.Cluster.Distances(d01,dCentroid0);
//OUTPUT(dDistances0, NAMED('dDistances0'));
dClosest0 := ML.Cluster.Closest(dDistances0);
//OUTPUT(dClosest0, NAMED('dClosest0'));

//*******add a data structure to keep track of  second closest center and best center
dTrack0 := DEDUP(SORT(DISTRIBUTE(dDistances0,x),x,value,LOCAL),x,KEEP 2,LOCAL);
//OUTPUT(dTrack0, NAMED('dTrack0'));

// dClosest0;
ub0_ini := dClosest0; 
//OUTPUT(ub0_ini, NAMED('ub0_ini'));

//get lbs0_ini
//lb of each group: every group should have at least two centroids**
lbs0_iniTemp := JOIN(dClosest0,dDistances0, LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y,RIGHT ONLY);
lbs0_iniTemp1 := SORT(JOIN(lbs0_iniTemp, Gt, LEFT.y = RIGHT.x, TRANSFORM(Mat.Types.Element,SELF.y := RIGHT.y; SELF := LEFT;)),x, y, value);
//********************correct the lbs0_ini
lbs0_ini := DEDUP(lbs0_iniTemp1,LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y);
// lbs0_ini := DEDUP(lbs0_iniTemp,x, RIGHT);
//OUTPUT(lbs0_iniTemp,NAMED('lbs0_iniTemp'));
//OUTPUT(lbs0_iniTemp1,NAMED('lbs0_iniTemp1')) ;
//OUTPUT(lbs0_ini,NAMED('lbs0_ini'));


//initialize Vcount0
//Get V
lVcount := RECORD
TYPEOF(Types.NumericField.id)  id := dClosest0.y;
TYPEOF(Types.NumericField.number) c := COUNT(group);
TYPEOF(Types.NumericField.value)   inNum:= 0;
TYPEOF(Types.NumericField.number) outNum := 0;
END;


Vin0temp := TABLE(dClosest0, {y; UNSIGNED c := COUNT(GROUP);},y );
//OUTPUT(Vin0temp, NAMED('Vin0temp'));
// Vin0_ini := PROJECT(Vin0temp, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.y; SELF.y := LEFT.c; SELF.value := 0; ));
// //OUTPUT(Vin0_ini, NAMED('Vin0_ini'));
V0_ini := PROJECT(Vin0temp, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.y; SELF.y := LEFT.c; SELF.value := 0; ));
//OUTPUT(V0_ini, NAMED('V0_ini'));

// Vout0_ini := Vin0_ini;



//running Kmeans on d01
KmeansD01 := ML.Cluster.KMeans(d01,dCentroid0,1,nConverge);

//*********************helper functions for calculate C'*****************************************
// Function to pull iteration N from a table of type lIterations
Types.NumericField dResult(UNSIGNED n=n,DATASET(lIterations) d):=PROJECT(d,TRANSFORM(Types.NumericField,SELF.value:=LEFT.values[n+1];SELF:=LEFT;));

    // Determine the delta along each axis between any two iterations
Types.NumericField tGetDelta(Types.NumericField L,Types.NumericField R):=TRANSFORM
      SELF.id:=IF(L.id=0,R.id,L.id);
      SELF.number:=IF(L.number=0,R.number,L.number);
      SELF.value:=R.value-L.value;
END;
dDelta(UNSIGNED n01=n-1,UNSIGNED n02=n,DATASET(lIterations) d):=JOIN(dResult(n01,d),dResult(n02,d),LEFT.id=RIGHT.id AND LEFT.number=RIGHT.number,tGetDelta(LEFT,RIGHT));
    
// Determine the distance delta between two iterations, using the distance
// method specified by the user for this module
dDistanceDelta(UNSIGNED n01=n-1,UNSIGNED n02=n,DATASET(lIterations) d):=FUNCTION
      iMax01:=MAX(dResult(n01,d),id);
      dDistance:=ML.Cluster.Distances(dResult(n01,d),PROJECT(dResult(n02,d),TRANSFORM(Types.NumericField,SELF.id:=LEFT.id+iMax01;SELF:=LEFT;)),fDist);
RETURN PROJECT(dDistance(x=y-iMax01),TRANSFORM({Types.NumericField AND NOT [number];},SELF.id:=LEFT.x;SELF:=LEFT;));
END;
ClusterPair:=RECORD
		Types.t_RecordID    id;
		Types.t_RecordID    clusterid;
		Types.t_FieldNumber number;
		Types.t_FieldReal   value01 := 0;
		Types.t_FieldReal   value02 := 0;
		Types.t_FieldReal   value03 := 0;
  END;
c_model := ENUM ( Dense = 1, SJoins = 2, Background = 4 );

MappedDistances(DATASET(Types.NumericField) d01,DATASET(Types.NumericField) d02 ,DF.Default Control = DF.Euclidean, DATASET(ClusterPair) dMap = DATASET([],ClusterPair)) := FUNCTION	
			// If we are in dense model then fatten up the records; otherwise zeroes not needed
   		df1 := IF( Control.Pmodel & c_model.dense > 0, Utils.Fat(d01), d01(value<>0) );
   		df2 := IF( Control.Pmodel & c_model.dense > 0, Utils.Fat(d02), d02(value<>0) );
   		// Construct the summary records used by SJoins and Background processing models
   		si1 := Control.SummaryID1(df1); // Summaries of each document by ID
   		si2 := Control.SummaryID2(df2); // May be used by any summary joins features
			//Slim down the si1 by filtering out the data points not in the dMap table
			si1_mapped := JOIN(dMap,si1, LEFT.id = RIGHT.id, TRANSFORM(Types.NumericField, SELF.number:=LEFT.clusterid; SELF := RIGHT; ));
			// Construct the 'background' matrix from the summary matrix
			bck := JOIN(si1_mapped,si2,LEFT.id<>RIGHT.id AND LEFT.number = RIGHT.id,TRANSFORM(Mat.Types.Element,SELF.x := LEFT.id, SELF.y := RIGHT.Id, SELF.value := Control.BackGround(LEFT,RIGHT)),ALL);
   		// Create up to two 'aggregate' numbers that the models may use
   		ex1 := Control.EV1(d01); 
   		ex2 := Control.EV2(d01);
   		//Mapped dataset of d01 and d02 based on the relation specified in the dMap table.
			df2_mapped := JOIN(dMap, df2, LEFT.clusterid = RIGHT.id, TRANSFORM(ClusterPair, SELF.clusterid := LEFT.id; SELF.value01:=RIGHT.value; SELF.value02:=0;SELF.value03:=0;SELF := RIGHT; ));//dRelation   
			ClusterPair Take2_mapped(df1 le,ClusterPair ri) := TRANSFORM
						SELF.clusterid := ri.id;
						SELF.id := le.id;
						SELF.number := le.number;
						SELF.value01 := Control.IV1(le.value,ri.value01);
						SELF.value02 := Control.IV2(le.value,ri.value01);
			END;
 			J := JOIN(df1,df2_mapped,LEFT.number=RIGHT.number AND LEFT.id<>RIGHT.id AND Control.JoinFilter(LEFT.value,RIGHT.value01,ex1) AND LEFT.id = RIGHT.clusterid ,Take2_mapped(LEFT,RIGHT),HASH); // numbers will be evenly distribute by definition
   		// Take all of the values computed for each matching ID and combine them
   		JG := GROUP(J,id,clusterid, ALL);  
   		ClusterPair  roll(ClusterPair le, DATASET(ClusterPair) gd) := TRANSFORM
   				SELF.Value01 := Control.Comb1(gd,ex1,ex2);
   				SELF.Value02 := Control.Comb2(gd,ex1,ex2); // These are really scratchpad
   				SELF.Value03 := Control.Comb3(gd,ex1,ex2);
   				SELF := le;
   		END;			  		
   		rld := ROLLUP(JG,GROUP,roll(LEFT,ROWS(LEFT)));
   		// In the SJoins processing model the si1/si2 data is now "passed to" the result - 01 first
   		J1 := JOIN(rld,si1,LEFT.id=RIGHT.id,TRANSFORM(ClusterPair,SELF.value01 := Control.Join11(LEFT,RIGHT),SELF.value02 := Control.Join12(LEFT,RIGHT),SELF.value03 := Control.Join13(LEFT,RIGHT), SELF := LEFT),LOOKUP);
   		J2 := JOIN(J1,si2,LEFT.clusterid=RIGHT.id,TRANSFORM(ClusterPair,SELF.value01 := Control.Join21(LEFT,RIGHT), SELF := LEFT),LOOKUP);
   		// Select either the 'normal' or 'post joined' version of the scores
   		Pro := IF ( Control.PModel & c_model.SJoins > 0, J2, rld );
   		ProAsDist := PROJECT(Pro,TRANSFORM(Mat.Types.Element,SELF.x := LEFT.id,SELF.y := LEFT.clusterid,SELF.Value := LEFT.Value01, SELF := LEFT));
   		// Now blend the scores that were computed with the background model
   		Mat.Types.Element blend(bck le,pro ri) := TRANSFORM
   		  SELF.value := Control.BackFront(le,ri);
   		  SELF := le;
   		END;   		
			BF := JOIN(bck,pro,LEFT.y=RIGHT.ClusterID AND LEFT.x=RIGHT.id,blend(LEFT,RIGHT),LEFT OUTER);
			
			// Either select the background blended version - or slim the scores down to a cluster distance
			RETURN IF(Control.PModel & c_model.Background>0, BF,ProAsDist); 	
			
  END; 	

//*********************************************************************************************************************************************************************

//set the result of Standard Kmeans to the result of first iteration
firstIte := KmeansD01.AllResults();
// //OUTPUT(firstIte,NAMED('firfostIte'));
firstResult := PROJECT(firstIte,TRANSFORM(Types.NumericField,SELF.value:=LEFT.values[2];SELF:=LEFT;)); 
//OUTPUT(firstResult,NAMED('firstResult'));

//*********************now Gt (deltaG will be initailized and updated in iteration ), ub, lb are initialized ************************
// Now join to the existing centroid dataset to add the new values to the end of the values set.
dAdded1:=JOIN(d02Prep,firstResult,LEFT.id=RIGHT.id AND LEFT.number=RIGHT.number,TRANSFORM(lIterations,SELF.values:=LEFT.values+[RIGHT.value];SELF:=RIGHT;),RIGHT OUTER);

//data preparation

//transform dCentroid to the format align with ub_ini, lbs0_ini, Vin0_ini, Vout0_ini
dCentroid0Trans := PROJECT(dCentroid0, TRANSFORM(Mat.Types.Element, SElF.value := LEFT.value ; SELF.x := LEFT.id; SELF.y := LEFT.number));


lInput:=RECORD 
TYPEOF(Types.NumericField.id) id;
TYPEOF(Types.NumericField.id) x;
TYPEOF(Types.NumericField.id) y;
SET OF TYPEOF(Types.NumericField.value) values;
END;

lInput transFormat(Mat.Types.Element input , UNSIGNED c) := TRANSFORM
SELF.id := c;
SELF.values := [input.value];
SELF := input;
END;

iCentroid0Temp := PROJECT(dCentroid0Trans, transFormat(LEFT, 1));
iCentroid0 := JOIN(iCentroid0temp, firstResult, LEFT.x = RIGHT.id AND LEFT.y = RIGHT.number, TRANSFORM(lInput, SELF.values := LEFT.values + [RIGHT.value]; SELF := LEFT;));

iUb0 := PROJECT(ub0_ini, transFormat(LEFT, 2));
iLbs0 := PROJECT(lbs0_ini, transFormat(LEFT, 3));
iV0 := PROJECT(V0_ini, transFormat(LEFT, 4));
input0 := iCentroid0 + iUb0 + iLbs0 ;
//OUTPUT(input0, NAMED('input0'));


//********************************************start iterations*************************************************
lInput yyfIterate(DATASET(lInput) d,UNSIGNED c):=FUNCTION
			
			dAddedx := PROJECT(d(id = 1), TRANSFORM(lIterations , SELF.id := LEFT. x; SELF.number:= LEFT.y; SELF.values := LEFT.values;));
			iUb := TABLE(d(id = 2), {x;y;values;});
			iLbs := TABLE(d(id = 3), {x;y;values;});
			// iV := TABLE(d(id = 4), {x;y;values;});
			
			ub0 := PROJECT(d(id = 2), TRANSFORM(Mat.Types.Element, SELF.value := LEFT.values[c]; SELF := LEFT;));
			//OUTPUT(ub0, NAMED('ub0'));
			lbs0 := PROJECT(d(id = 3), TRANSFORM(Mat.Types.Element, SELF.value := LEFT.values[c]; SELF := LEFT;));
			//OUTPUT(lbs0, NAMED('lbs0'));
			V0 := PROJECT(d(id = 4), TRANSFORM(Mat.Types.Element, SELF.value := LEFT.values[c]; SELF := LEFT;));
			//OUTPUT(V0, NAMED('V0'));
			
			//*********add lb0 which is the min(lbs0) of each data point
			lb0 := DEDUP(SORT(lbs0, x, y, value), x);
			//OUTPUT(lb0, NAMED('lb0_globallb'));
			//lbG := MIN(lb0, value);
			
			//calculate the deltaC
			deltac := dDistanceDelta(c,c-1,dAddedx);
			//OUTPUT(deltac,NAMED('deltac'));
			
			bConverged := IF(MAX(deltac,value)<=nConverge,TRUE,FALSE);
			//OUTPUT(bConverged, NAMED('iteration1_converge'));
			
			dCentroid1 := PROJECT(dAddedx,TRANSFORM(Types.NumericField,SELF.value:=LEFT.values[c+1];SELF:=LEFT;));
			//OUTPUT(dCentroid1,NAMED('dCentroid1'));

			//get deltaG1
			//group deltacg by Gt: first JOIN(deltaC with Gt) then group by Gt
			deltacg :=JOIN(deltac, Gt, LEFT.id = RIGHT.x, TRANSFORM(Mat.Types.Element,SELF.value := LEFT.value; SELF := RIGHT;));
			//OUTPUT(deltacg, NAMED('delatcg'));
			deltacGt := SORT(deltacg, y,-value);
			//OUTPUT(deltacGt,NAMED('deltacGt'));
			deltaG1 := DEDUP(deltacGt,y);
			//OUTPUT(deltaG1,NAMED('deltaG1')); 

			//update ub0 and lbs0
			ub1_temp := JOIN(ub0, deltac, LEFT.y = RIGHT.id, TRANSFORM(Mat.Types.Element, SElF.value := LEFT.value + RIGHT.value; SELF := LEFT;));
			//OUTPUT(ub1_temp, NAMED('ub1_temp'));
			lbs1_temp := JOIN(lbs0, deltaG1, LEFT.y = RIGHT.y , TRANSFORM(Mat.Types.Element, SELF.value := ABS(LEFT.value - RIGHT.value); SELF := LEFT));
			//OUTPUT(lbs1_temp, NAMED('lbs1_temp'));
			
			
		
			// groupfilter1
			//calculate the 'b(x) for all x
			dMap1 := PROJECT(ub0, TRANSFORM(ClusterPair, SELF.id := LEFT.x; SELF.clusterid := LEFT.y; SELF.number := 0; SELF.value01 := LEFT.value; SELF.value02 := 0; SELF.value03 := 0;));		
			dMappedDistances1 := SORT(MappedDistances(d01,dCentroid1,fDist,dMap1), x, value);	
			//OUTPUT(dMappedDistances1, NAMED('dMappedDistances1'));
			ub1_changed_temp := ML.Cluster.Closest(dMappedDistances1);
			//OUTPUT(ub1_changed_temp);
      groupFilter1 := JOIN(lbs1_temp, ub1_temp,LEFT.x = RIGHT.x AND (LEFT.value < RIGHT.value), TRANSFORM(Mat.Types.Element, SELF := LEFT));	
      //OUTPUT(groupFilter1, NAMED('groupFilter1'));			
			
			dMap2_temp := JOIN(groupFilter1, Gt, LEFT.y = RIGHT.y, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.x, SELF.y := RIGHT.x, SELF.value := LEFT.value ));
			dMap2 := PROJECT(dMap2_temp, TRANSFORM(ClusterPair, SELF.id := LEFT.x; SELF.clusterid := LEFT.y; SELF.number := 0; SELF.value01 := LEFT.value; SELF.value02 := 0; SELF.value03 := 0;));		
      //OUTPUT(dMap2, NAMED('dMap2'));			
			dMappedDistances2 := SORT(MappedDistances(d01,dCentroid1,fDist,dMap2), x, value);	
			//OUTPUT(dMappedDistances2, NAMED('dMappedDistances2'));
			ub1_changed_final := ML.Cluster.Closest(dMappedDistances2);
			//OUTPUT(ub1_changed_final, NAMED('ub1_changed_final'));
			ub1_changed := JOIN(ub1_changed_temp, ub1_changed_final, LEFT.x = RIGHT.x AND LEFT.value > RIGHT.value, TRANSFORM(RIGHT));
			 
			//OUTPUT(ub1_changed, NAMED('ub1_changed'));
			ub1_unchanged := JOIN(ub1_changed_temp, ub1_changed, LEFT.x = RIGHT.x, TRANSFORM(LEFT),LEFT ONLY);
			//OUTPUT(ub1_unchanged, NAMED('ub1_unchanged'));
			ub1 := SORT(ub1_changed + ub1_unchanged, x, y, value);
      //OUTPUT(ub1, NAMED('ub1'));
							
			lbs1_changed_temp := JOIN(dMappedDistances1, ub1_changed, LEFT.x = RIGHT.x, TRANSFORM(LEFT), LEFT ONLY);
			lbs1_changed_temp1 := JOIN(lbs1_changed_temp, Gt, LEFT.y = RIGHT.x, TRANSFORM(Mat.Types.Element, SELF.x := LEFT.x, SELF.y := RIGHT.y, SELF.value := LEFT.value ));
			//OUTPUT(lbs1_changed_temp, NAMED('lbs1_changed_temp'));
			lbs1_changed:= DEDUP(SORT(lbs1_changed_temp1,x,y,value),x,y);
			lbs1_unchanged:= JOIN(lbs1_temp, lbs1_changed, LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y, LEFT ONLY);
      lbs1 := lbs1_unchanged + lbs1_changed;			
			//OUTPUT(lbs1, NAMED('lbs1'));
			
      dClusterCounts:=TABLE(ub1,{y;UNSIGNED c:=COUNT(GROUP);},y,FEW);
			//OUTPUT(dClusterCounts, NAMED('dClusterCounts'));
      // Join closest to the document set and replace the id with the centriod id
      dClustered:=SORT(DISTRIBUTE(JOIN(d01,ub1,LEFT.id=RIGHT.x,TRANSFORM(Types.NumericField,SELF.id:=RIGHT.y;SELF:=LEFT;),HASH),id),RECORD,LOCAL);
      // Now roll up on centroid ID, summing up the values for each axis
      dRolled:=ROLLUP(dClustered,TRANSFORM(Types.NumericField,SELF.value:=LEFT.value+RIGHT.value;SELF:=LEFT;),id,number,LOCAL);
      // Join to cluster counts to calculate the new average on each axis
      dJoined:=JOIN(dRolled,dClusterCounts,LEFT.id=RIGHT.y,TRANSFORM(Types.NumericField,SELF.value:=LEFT.value/RIGHT.c;SELF:=LEFT;),LOOKUP);
      // Find any centroids with no document allegiance and pass those through also
		  dPass:=JOIN(dCentroid1,TABLE(dJoined,{id},id,LOCAL),LEFT.id=RIGHT.id,TRANSFORM(LEFT),LEFT ONLY,LOOKUP);
			dCentroidout := dPass + dJoined;
      // //OUTPUT(dCentroid2, NAMED('dCentroid2'));  
			//Now join to the existing input datasets to add the new values to the end of each values set.
			newCsTemp := JOIN(dAddedx, dCentroidOut, LEFT.id = RIGHT.id AND LEFT.number=RIGHT.number,TRANSFORM(lIterations,SELF.values:=LEFT.values+[RIGHT.value];SELF:=LEFT;));
			dCentroidsOut := PROJECT(newCsTemp, TRANSFORM(lInput,SELF.id := 1;SELF.values:=LEFT.values;SELF.y := LEFT.number; SELF.x:=LEFT.id;));					
			dUbOut := JOIN(iUb, ub1, LEFT.x = RIGHT.x ,TRANSFORM(lInput,SELF.id := 2;SELF.values:=LEFT.values+[RIGHT.value];SELF.y := RIGHT.y; SELF:=LEFT;));
			dLbsOut := JOIN(iLbs, lbs1, LEFT.x = RIGHT.x ,TRANSFORM(lInput,SELF.id := 3;SELF.values:=LEFT.values+[RIGHT.value];SELF.y := RIGHT.y; SELF:=LEFT;));
			// dClusterCountout := JOIN(dClusterCountsIn, dClusterCountsUpdate, LEFT.x = RIGHT.x ,TRANSFORM(lInput,SELF.id := 4;SELF.values:=LEFT.values+[RIGHT.value];SELF.y := RIGHT.y; SELF:=LEFT;));
					//Integrate each dataset into one dataset as the output dataset
			dOutput := dCentroidsOut+ dUbOut + dLbsOut; 		
		RETURN IF( MAX(deltac,value)<=nConverge OR COUNT(groupFilter1)=0,  d , dOutput );

		END;
		dIterationResults :=LOOP(input0,n-1,yyfIterate(ROWS(LEFT),COUNTER));
		dResults := TABLE(dIterationResults(id=1), {x, TYPEOF(Types.NumericField.number) number := y, values});
		SHARED dIterations:=IF(iOffset>0,PROJECT(dResults,TRANSFORM(lIterations,SELF.id:=LEFT.x-iOffset;SELF.number :=LEFT.number; SELF := LEFT;)),dResults):INDEPENDENT;

		//Show the fully traced result set
		EXPORT lIterations AllResults:=dIterations;

	END;		
