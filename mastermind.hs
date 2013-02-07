
import System.Random
import System.IO
import Data.Char

{- Integrantes:
  	Kevin Silva
		Stella Andrade
		Danny Ponce
-}


main:: IO()
main=do
   gen1 <- getStdGen
   gen2 <- getStdGen
   f <- readFile "clave.txt"
   codigo <- stringToList(f)
   let str = "************* INTENTOS MASTERMIND ************\n"
   writeFile "intentos.txt" (str)
   let lista6_1 = aleatorios(gen1,10000000,1,6)     
   let inicialCode = take 4 lista6_1
   let lista6_2 = drop 4 lista6_1
   let lista3_1 = aleatorios(gen2,10000000,0,3)
   let cfg = compara(codigo,[1,3,4,2])
   let nu = 1
   print(nu)  
   print(cfg)
   let numero = nu+1
   let st =  show cfg
   appendFile "intentos.txt" ("#"++show(nu)++" "++st ++ "\n")
   createCode(cfg,codigo,lista6_2,lista3_1,numero)
   

{- Funcion que nos devuelve ua lista de numeros aleatorios-} 
aleatorios:: (StdGen,Int,Int,Int)->[Int]       
aleatorios (gen,n,min,max) = do
	let (nroAleat, newGen) = randomR (min,max) gen :: (Int, StdGen)   
	if n==1 then [n]
	else [nroAleat]++aleatorios(newGen,n-1,min,max)               

{-Funcion que realiza el algoritmmo descrito el el paper, imprime el codigo y termina llamandose a si misma-}
createCode::(([Int],(Int,Int)),[Int],[Int],[Int],Int)->IO()
createCode (cfg,code,al6,al3,num) = do
    		let x=fst(snd(cfg))  
		let indicesX = indNumerosX(x,al3)
   		let al3_1 = drop 4 al3
   		let pocode = newGuessXStep1(fst(cfg),indicesX,[0,0,0,0])
   		let y=snd(snd(cfg))
  		let indicesY = indNumerosY(y,al3_1,indicesX)
                let al3_2 = drop 4 al3_1
   		let pocode2 = newGuessYStep2(fst(cfg),indicesY,pocode,al3_2,indicesX)	
                let al3_3 = drop 4 al3_2
                let pocode3 = newGuessStep3(al6,pocode2)
		let al6_1 = drop 4 al6
		let posibleCFG = compara(code,pocode3)  
		print(num)
		print(posibleCFG)	
		let nuevoCFG = newCFG(cfg,posibleCFG)
		let distancia = transformaXYaMeta(snd(nuevoCFG))		
		let cad =  show posibleCFG
                appendFile "intentos.txt" ("#"++show(num)++" "++cad++ "\n")	
		if(distancia==13) then putStrLn $ "GANASTE! "		
		else createCode(nuevoCFG,code,al6_1,al3_3,num+1)

{-Funcion que nos devuelve el numero de digitos que se encuentran en la posicion correcta -}
compX::([Int],[Int])->Int
compX (code,posibleCode) = if(null(code)) then 0 
		           else if(head(code)==head(posibleCode)) then 1 + compX(tail(code),tail(posibleCode))
				else 0 + compX(tail(code),tail(posibleCode))


{-Funcion que nos devuelve el numero de digitos que se encuentran en el codigo, pero en la posicion incorrecta -}
compY::([Int],[Int])->Int
compY (code,posibleCode) = do
			   let n = head(posibleCode)
		           if(null(posibleCode)) then 0 
		           else if(elem n code) then 1 + compY(code,tail(posibleCode))
				else 0 + compY(code,tail(posibleCode))

{-Funcion que devuelve la lista con el codigo y la tupla correpondiente a los digitos correctos e incorrectos-}
compara::([Int],[Int])->([Int],(Int,Int))
compara (code,posibleCode) =  (posibleCode,(compX(code,posibleCode),compY(code,posibleCode)-compX(code,posibleCode)))


indNumerosX::(Int,[Int])->[Int]
indNumerosX (n,aleat4)  = if(n==1) then [head(aleat4)]
			       else [head(aleat4)]++indNumerosX(n-1,tail(aleat4))
{-Paso 1 correspondiente al algoritmo descrito el el paper-}
newGuessXStep1::([Int],[Int],[Int])->[Int]
newGuessXStep1 (cfg,indX,newGuess) = do
			let n=head(indX)
			let num = cfg !!n
                        let x = n+1
                        let headlist = take n newGuess
			let taillist = drop x newGuess
			let newlist = headlist ++ [num] ++ taillist 
			if (length(indX)==1) then newlist
			else newGuessXStep1(cfg,tail(indX),newlist)

{-Paso 2 correspondiente al algoritmo descrito el el paper-}
newGuessYStep2:: ([Int],[Int],[Int],[Int],[Int])->[Int]
newGuessYStep2 (cfg,indy,newGuess,aleat,listaIndX) = do
		let n = head(indy)
                let num = cfg !!n
                let a = head(aleat)		
		let x = a+1	 
                let headlist = take a newGuess
		let taillist = drop x newGuess
		let newlist = headlist ++ [num] ++ taillist 
		if(elem a listaIndX) then newGuessYStep2(cfg,indy,newGuess,tail(aleat),listaIndX)
                else if (length(indy)==1) then newlist
	              else newGuessYStep2(cfg,tail(indy),newlist,tail(aleat),listaIndX)



indNumerosY::(Int,[Int],[Int])->[Int]
indNumerosY (n,aleat4,listaIndX)  = do
		let a = head(aleat4)
		if(elem a listaIndX) then []++indNumerosY(n,tail(aleat4),listaIndX)
	        else if(n==1) then [a]
		      else [a]++indNumerosY(n-1,tail(aleat4),listaIndX)
{-Paso 3 correspondiente al algoritmo descrito el el paper-}
newGuessStep3:: ([Int],[Int])->[Int]
newGuessStep3 (aleat,code) = do
		let x = head(aleat)
		let n = head(code)
		if(null(code)) then []
		else if(n==0) then [x]++newGuessStep3(tail(aleat),tail(code))
		     else [n]++newGuessStep3(aleat,tail(code))
		

{-Comprueba si el posible new favourite guess es el ideal-}

newCFG::(([Int],(Int,Int)),([Int],(Int,Int)))->([Int],(Int,Int))
newCFG (cfg,possiblecfg) = if (transformaXYaMeta(snd(cfg))>=transformaXYaMeta(snd(possiblecfg))) then cfg 
			   else possiblecfg



{-Mapea del resultado de la tupla (x,y) a su respectiva cercanía con la meta-}

transformaXYaMeta::(Int,Int)->Int
transformaXYaMeta tupla
	|(fst(tupla)==0)&&(snd(tupla)==0)=0
	|(fst(tupla)==0)&&(snd(tupla)==1)=1
	|(fst(tupla)==1)&&(snd(tupla)==0)=2
	|(fst(tupla)==0)&&(snd(tupla)==2)=3
	|(fst(tupla)==1)&&(snd(tupla)==1)=4
	|(fst(tupla)==2)&&(snd(tupla)==0)=5
	|(fst(tupla)==0)&&(snd(tupla)==3)=6
	|(fst(tupla)==1)&&(snd(tupla)==2)=7
	|(fst(tupla)==2)&&(snd(tupla)==1)=8
	|(fst(tupla)==3)&&(snd(tupla)==0)=9
	|(fst(tupla)==0)&&(snd(tupla)==4)=10
	|(fst(tupla)==1)&&(snd(tupla)==3)=11
	|(fst(tupla)==2)&&(snd(tupla)==2)=12
	|(fst(tupla)==4)&&(snd(tupla)==0)=13
	|otherwise = -1


{-Transforma de String a [Int]-}

stringToList :: String -> IO [Int]
stringToList = readIO
