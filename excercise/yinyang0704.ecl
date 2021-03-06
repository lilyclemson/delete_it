﻿IMPORT * FROM $;
IMPORT Std.Str AS Str;
IMPORT TS;
IMPORT ML.Mat;
IMPORT ML;
IMPORT ML.Cluster.DF as DF;
IMPORT ML.Types AS Types;
IMPORT ML.Utils;

lMatrix:={UNSIGNED id;REAL x;REAL y;};

dDocumentMatrix:=DATASET([
{1,2,2},
{2,4,2},
{3,6,2},
{4,9,2}
],lMatrix);

dCentroidMatrix:=DATASET([
{1,3,2},
{2,7,2}
],lMatrix);


ML.ToField(dDocumentMatrix,dDocuments);
ML.ToField(dCentroidMatrix,dCentroids);


//Inputs:
d01 := dDocuments;
d02 := dCentroids;
n := 10;
nConverge := 0;



//***************************************data preparation*********************************

// internal record structure of d02
lIterations:=RECORD 
TYPEOF(Types.NumericField.id) id;
TYPEOF(Types.NumericField.number) number;
SET OF TYPEOF(Types.NumericField.value) values;
END;



//make sure all ids are different
iOffset:=IF(MAX(d01,id)>MIN(d02,id),MAX(d01,id),0);

//transfrom the d02 to internal data structure
d02Prep:=PROJECT(d02,TRANSFORM(lIterations,SELF.id:=LEFT.id+iOffset;SELF.values:=[LEFT.value];SELF:=LEFT;));

d02Prep; // *********************************result 1

//c = the number of iterations. It will increase by 1 after each iteration.
c := 1;

// set the current centroids to the results of the most recent iteration
dCentroid := PROJECT(d02Prep,TRANSFORM(Types.NumericField,SELF.value:=LEFT.values[c];SELF:=LEFT;));

dCentroid; //*********************************result 2


//*********************************Initialization Part******************************************************	

//step 1: get Gt 
//running Kmeans on dCentroid
//inputs

K := COUNT(DEDUP(d02,id));
K; //******************************** result 3
t:= IF(K/10<1, 1, K/10);
t; //******************************** result 4


//temporary solution to get dt is that dt = the first t cendroids of dCentroid 
temp := t * 2;
tempDt := dCentroid[1..temp];
//transform ids of the centroids of dt starting from 1....
Types.NumericField transDt(Types.NumericField L, INTEGER C) := TRANSFORM
SELF.id := ROUNDUP(C/2);
SELF := L;
END;
dt := project(tempDt, transDt(LEFT, COUNTER));
dt;//******************************** result 5

nt := n;
tConverge := nConverge;

//run Kmeans on dCentroid
KmeansDt :=ML.Cluster.KMeans(dCentroid,dt,nt,tConverge);
// PreGroups := KmeansDt.Allegiances();//the assignment of each centroid to a group
// PreGroups;//**************result 6****** Gt
Gt := TABLE(KmeansDt.Allegiances(), {x,y});//the assignment of each centroid to a group
Gt;//**************result 6 ******deltaGt can get from JOIN with deltaC


//Step 2: get ub and lb for each data point
//get ub
//running Kmeans on d01
KmeansD01 := ML.Cluster.KMeans(d01,dCentroid,1,nConverge);
ub := KmeansD01.Allegiances();
ub; //***************result 7 ********ub == dClosest

// SET of the value of ub, useful for next iteration to update c
ubValue := DEDUP(TABLE(ub,{y},y),y); //  
ubValue; //***************result 8 ************dClosest.value


//get lb
//all the distances from each data point to each centroid
dDistances:= ML.Cluster.Distances(d01,dCentroid);
joinlb := JOIN(ub,dDistances, LEFT.x = RIGHT.x AND LEFT.y = RIGHT.y,RIGHT ONLY);
joinlb;//***************result 9 ==  dDistances - dClosest

//Join Gt and dDistances to get the lb of each group
lBound := RECORD
TYPEOF(Types.NumericField.id) xid;
TYPEOF(Types.NumericField.id) cid;
TYPEOF(Types.NumericField.id) gid;
TYPEOF(Types.NumericField.value) lb;
END;


lBound transLbound(joinlb L, Gt R) := TRANSFORM
SELF.xid := L.x;
SELF.cid := L.y;
SELF.gid := R.y;
SELF.lb := L.value;
END;

//join result 8 and result 5 to get the dataset for lb
jDistG := JOIN(joinlb, Gt, LEFT.y = RIGHT.x, transLbound(LEFT,RIGHT));
jDistG;//***************result 10**************all the lbs without group by Gt

//lb of each group

// jDistGv := DEDUP(SORT(GROUP(jDistG,xid,gid),lb),gid);
// jDistGv ;//***************result 11 == lb of each group

lbs := DEDUP(SORT(GROUP(jDistG,xid,gid),lb),gid);
lbs ;//***************result 11 == lb of each group

//*********************now Gt (deltaG will be initailized and updated in iteration ), ub, lb are initialized ************************

//*********************************Iteration Part******************************************************	
//get deltaC
//step 1: update c --> c' according to equation 1 --> c' = (c* V + Vin - Vout)/Vnew
//Define the RECORD Structure of V
lV := RECORD
TYPEOF(Types.NumericField.id) id;
TYPEOF(Types.NumericField.id) xid;
END;
			 		
//initialize Vin and Vout
//Vin
lV transVin(ub L) := TRANSFORM
SELF.id := L.y;
SELF.xid := 0;
END;
Vin := PROJECT(ub, transVin(LEFT));
Vin; //**********************************result 12
		
//Vout
lV transVout(ub L) := TRANSFORM
SELF.id := L.y;
SELF.xid := 0;
END;		
Vout := PROJECT(ub, transVout(LEFT));		
Vout; //**********************************result 13
	
//Get V
//calculate the number of documents allied to each centroid
// dClusterCounts:=TABLE(ub,{y;UNSIGNED c:=COUNT(GROUP);},y,FEW);
// dClusterCounts; //************************result 14
V:=TABLE(ub,{y;UNSIGNED c:=COUNT(GROUP);},y,FEW);
V; //************************result 14

//get c*V
// Join ub to the document set and replace the id with the centriod id
dClusteredV:=SORT(DISTRIBUTE(JOIN(d01,ub,LEFT.id=RIGHT.x,TRANSFORM(Types.NumericField,SELF.id:=RIGHT.y;SELF:=LEFT;),HASH),id),RECORD,LOCAL);
// Now roll up on centroid ID, summing up the values for each axis for V
dRolledV:=ROLLUP(dClusteredV,TRANSFORM(Types.NumericField,SELF.value:=LEFT.value+RIGHT.value;SELF:=LEFT;),id,number,LOCAL);

dRolledV;// ***********************************************************result15


//get Vin
// Join Vin to the document set and replace the id with the centriod id
dClusteredVin:=SORT(JOIN(d01,Vin,LEFT.id=RIGHT.xid,TRANSFORM(Types.NumericField,SELF.id:=RIGHT.id;SELF:=LEFT;)),id);	
// Now roll up on centroid ID, summing up the values for each axis for Vin
dRolledVin:=ROLLUP(dClusteredVin,TRANSFORM(Types.NumericField,SELF.value:=LEFT.value+RIGHT.value;SELF:=LEFT;),id,number,LOCAL);
dRolledVin;// ***********************************************************result16

//get Vout	
// Join Vout to the document set and replace the id with the centriod id
dClusteredVout:=SORT(JOIN(d01,Vout,LEFT.id=RIGHT.xid,TRANSFORM(Types.NumericField,SELF.id:=RIGHT.id;SELF:=LEFT;)),id);
// Now roll up on centroid ID, summing up the values for each axis for Vout
dRolledVout:=ROLLUP(dClusteredVout,TRANSFORM(Types.NumericField,SELF.value:=LEFT.value+RIGHT.value;SELF:=LEFT;),id,number,LOCAL);
dRolledVout;// ***********************************************************result17

Types.NumericField transVVin(dRolledVin v, dRolledVout vin) := TRANSFORM
SELF.value := v.value + vin.value;
SELF := v;
END;
dJoinedVVin:=JOIN(dRolledV,dRolledVin,LEFT.id = RIGHT.id AND LEFT.number=RIGHT.number,transVVin(left,right), LEFT OUTER);
dJoinedVVin;// ***********************************************************result18

Types.NumericField transVVout(dJoinedVVin vvin, dRolledVout vout) := TRANSFORM
SELF.value := vvin.value - vout.value;
SELF := vvin;
END;
dJoinedVVout:=JOIN(dJoinedVVin,dRolledVout,LEFT.id = RIGHT.id AND LEFT.number=RIGHT.number,transVVout(left,right),LEFT OUTER);
dJoinedVVout;// ***********************************************************result18

//Join to cluster counts to calculate the new average on each axis for C'
dJoined:=JOIN(dJoinedVVout,V,LEFT.id=RIGHT.y,TRANSFORM(Types.NumericField,SELF.value:=LEFT.value/RIGHT.c;SELF:=LEFT;),LOOKUP);
dJoined;
				
// Find any centroids with no document allegiance and pass those through also
dPass:=JOIN(dCentroid,TABLE(ub,{y},y,LOCAL),LEFT.id=RIGHT.y,TRANSFORM(LEFT),LEFT ONLY,LOOKUP);
dPass;// ***********************************************************result21

// Now join to the existing centroid dataset to add the new values to
// the end of the values set.
dAdded:=JOIN(d02Prep,dJoined + dPass,LEFT.id=RIGHT.id AND LEFT.number=RIGHT.number,TRANSFORM(lIterations,SELF.values:=LEFT.values+[RIGHT.value];SELF:=RIGHT;),RIGHT OUTER);
dAdded;// ***********************************************************result22


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
fDist :=DF.Euclidean;
dDistanceDelta(UNSIGNED n01=n-1,UNSIGNED n02=n,DATASET(lIterations) d):=FUNCTION
      iMax01:=MAX(dResult(n01,d),id);
      dDistancell:=ML.Cluster.Distances(dResult(n01,d),PROJECT(dResult(n02,d),TRANSFORM(Types.NumericField,SELF.id:=LEFT.id+iMax01;SELF:=LEFT;)),fDist);
RETURN PROJECT(dDistancell(x=y-iMax01),TRANSFORM({Types.NumericField AND NOT [number];},SELF.id:=LEFT.x;SELF:=LEFT;));
END;

//*****************************************************************************************************



// Check the distance delta for the last two iterations.  If the highest
// value is below the convergence threshold, then set bConverged to TRUE
bConverged:=IF(c=1,FALSE,MAX(dDistanceDelta(c-1,c-2,d02Prep),value)<=nConverge);

// If the centroids have converged, simply pass the input dataset through
// to the next iteration.  Otherwise perform an iteration.
result :=IF(bConverged,d02Prep,dAdded);
result; // ***********************************************************result23

//iteration 2
c1 := 2;
//Groupfilter
//get deltaC
deltaC := dDistanceDelta(c1-1,c1-2,dAdded);
deltaC; // ***********************************************************result24
//RECORD structure of lGroup
lGroup := RECORD
TYPEOF(Types.NumericField.id) id;
TYPEOF(Types.NumericField.id) cid;
TYPEOF(Types.NumericField.value) value;
END;
//group deltaC by Gt: first JOIN(deltaC with Gt) then group by Gt
deltaCg :=JOIN(deltaC, Gt, LEFT.id = RIGHT.x, TRANSFORM(lGroup,SELF.id := RIGHT.y; SELF.cid := LEFT.id; SELF := LEFT));
deltaCg;// ***********************************************************result25
//RECORD structure of deltaG
ldeltaG := RECORD
deltaCg.id;
TYPEOF(Types.NumericField.value) value := MAX(GROUP, deltaCg.value);
END;
deltaG := TABLE(deltaCg, ldeltaG, id );
deltaG;// ***********************************************************result26

//**************************Now we got deltaC and deltaG *****************************
//apply Lemma1 to each data point to see if they have to change centroid
lGroupFilter := RECORD
TYPEOF(Types.NumericField.id) id;
TYPEOF(Types.NumericField.value) ubUpdate;
TYPEOF(Types.NumericField.value) lbUpdate;
BOOLEAN pass;
END;

//ubUpdate;
lUbUpdate := RECORD
TYPEOF(Types.NumericField.id) id;
TYPEOF(Types.NumericField.id) cid;
TYPEOF(Types.NumericField.value) ub;
TYPEOF(Types.NumericField.value) ubUpdate;
END;
ubUpdate := JOIN(ub,deltaC, LEFT.y = RIGHT.id, TRANSFORM(lUbUpdate, SELF.id := LEFT.x; SELF.cid := LEFT.y ; SELF.ub := LEFT.value; SELF.ubUpdate:= LEFT.value+ RIGHT.value) );
//lbUpdate
// lGroupFilter transLemma1(Types.NumericField d, lBound lbs, deltaC dc, deltaG dg) := TRANSFORM
// SELF.id := d.id ;
// SELF.left := 

// END;
ubUpdate;
//lbUpdate;
lLbUpdate := RECORD
TYPEOF(Types.NumericField.id) xid;
TYPEOF(Types.NumericField.id) gid;
TYPEOF(Types.NumericField.value) lb;
TYPEOF(Types.NumericField.value) lbUpdate;
END;
lbUpdate := JOIN(lbs,deltaG, LEFT.gid = RIGHT.id, TRANSFORM(lLbUpdate, SELF.lbUpdate := LEFT.lb + RIGHT.value; SELF := LEFT));
lbUpdate;
lGroupFilter transLemma1(ubUpdate L, lbUpdate R) := TRANSFORM
SELF.id := L.id;
SELF.ubUpdate :=  L.ubUpdate;
SELF.lbUpdate := R.lbUpdate;
SELF.pass := IF(L.ubUpdate < R.lbUpdate, FALSE, 
																	IF(L.ub < R.lbUpdate, FALSE,TRUE));
END;

groupFilter := JOIN(ubUpdate, lbUpdate, LEFT.id = RIGHT.xid, transLemma1(LEFT, RIGHT));
// groupFilter;
countGF := COUNT(groupFilter(pass = TRUE));
results := IF(countGF >0,countGF,0);
results;
dAdded;


