library dsmodel203;
  {-Model van de berging in een gebied waarin het peil wordt gereguleerd door middel
    van een schotbalk stuw en wateraanvoer met een gemaal. De berging vindt plaats in opper-
    vlaktewater, evt. vermeerderd met de berging in de onverzadigde zone.
    De relatie tussen de berging en de waterstand wordt gespecificeerd door middel van een
    tabel. Het neerslagoverschot wordt berekend als de som van het neerslagoverschot uit
    open water en het neerslagoverschot van gras. De verhouding (neerslagoverschot open water/
    neerslagoverschot gras) wordt bepaald door de fractie van het waterbalansgebied
    dat droogvalt. In verband hiermee wordt de relatie tussen de geborgen hoeveelheid water
    en de fractie van het waterbalansgebied waar sprake is van innundatie gespecificeerd.
    Als een gespecificeerde minimale waterstand wordt bereikt, dan wordt de (eveneens gespecificeerde)
    maximale wateraanvoercapaciteit gebruikt om het waterniveau weer op peil te brengen.
    De relatie met de omgeving van het waterbalansgebied wordt opgegeven door middel van
    een tabel waarin het verband is beschreven tussen de waterstand en de toestroming van grondwater
    (vertikaal = kwel en lateraal). Voorbeeldproject: 'Onderzoek waterpeilen en -fluctuaties Dannemeer',
    DLG 2007}

  { Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  ShareMem,
  windows, SysUtils, Classes, LargeArrays, ExtParU, USpeedProc, uDCfunc,
  UdsModel, UdsModelS, xyTable, DUtils, uError, Math;

Const
  cModelID      = 203;  {-Uniek modelnummer}

  {-Beschrijving van de array met afhankelijke variabelen}
  cNrOfDepVar   = 7;    {-Lengte van de array met afhankelijke variabelen}
  cBerging      = 1;    {-Berging in in oppervlaktewater, evt. vermeerderd met de berging in de onverzadigde zone (m)}
  cCumOv        = 2;    {-Afvoer over de stuw (cumulatief, m)}
  cCumWs        = 3;    {-Cum. waterstand (voor berekening gem. waterstand, m+NAP)}
  cCumKwel      = 4;    {-Toestroming naar berging via grondwater (kwel plus laterale toestroming; cumulatief, m)}
  cCumQWa       = 5;    {-Wateraanvoer (cumulatief, m)}
  cCumNN        = 6;    {-Neerslagoverschot (cumulatief, m)}
  cHuidWaDuur   = 7;    {-Duur van huidige wateraanvoer (interne parameter, d)}

  {-Aantal keren dat een discontinuiteitsfunctie wordt aangeroepen in de procedure met
    snelheidsvergelijkingen (DerivsProc)}
  nDC = 0;

  {-Variabelen die samenhangen met het aanroepen van het model vanuit de Shell}
  cnRP = 4;   {-Aantal RP-tijdreeksen die door de Shell moeten worden aangeleverd (in
                de externe parameter Array EP (element EP[ indx-1 ]))}
  cnSQ = 0;   {-Idem punt-tijdreeksen}
  cnRQ = 0;   {-Idem lijn-tijdreeksen}

  {-Beschrijving van het eerste element van de externe parameter-array (EP[cEP0])}
  cNrXIndepTblsInEP0 = 8; {-Aantal XIndep-tables in EP[cEP0]}
  cNrXdepTblsInEP0   = 0;  {-Aantal Xdep-tables   in EP[cEP0]}

  {-Nummering van de xIndep-tabellen in EP[cEP0].xInDep De nummers 0&1 zijn gereserveerd}
  cTb_MinMaxValKeys    = 2; {-Grenzen aan key-values}
  cTb_Berging_Ws       = 3; {-Beschr. v.d. relatie tussen de berging (V, (m)) en de waterstand (Ws, (m+NAP))}
  cTb_Ws_FrOppWtr      = 4; {-Beschr. v.d. relatie tussen de waterstand (Ws, (m+NAP)) en de fractie open water (-)}
  cTb_Ws_Kwel          = 5; {-Beschr. v.d. relatie tussen de waterstand (Ws, (m+NAP)) en de toestroming van grondwater (m/d)}
  cTb_StuwPars         = 6; {-Param. voor de beschrijving van de stuw (c, breedte, kruinniveau afw.opp., max. overstorthoogte)}
  cTb_WaPars           = 7; {-Param. voor het beschrijven van de wateraanvoer (min./max. niveau)}

  {-Nummering van de xIndep-tabellen in EP[cEP0].xDep}
  {-}

  {-Beschrijving van het tweede element van de externe parameter-array (EP[cEP1])}
  {-Opmerking: table 0 van de xIndep-tabellen is gereserveerd}

  {-Nummering van de xdep-tabellen in EP[cEP1]}
  cTb_NNgras      = 0; {-Neerslagoverschot gras(m/d)}
  cTb_NNopenWater = 1; {-Neerslagoverschot open water(m/d)}
  cTb_WaCap       = 2; {-Capaciteit wateraanvoer (m/d)}
  cTb_Ws_Init     = 3; {-Initiele waterstand (m+NAP)}

  {-Model specifieke fout-codes: -8051..-8099}
  cInvld_NN                            = -8051; {-Ongeldige waarde van het neerslagoverschot (m/d)}
  cInvld_WaCap                         = -8052; {-Ongeldige waarde voor de wateraanvoercapaciteit (m/d)}
  cInvld_Ws                            = -8053; {-Ongeldige waterstand (m+NAP)}
  cH1Exceeded                          = -8054; {-Overstorthoogte te groot (>cH1Max) }
  cErrorCreatingTable_Berging_Ws       = -8055; {-fout bij het maken van tabel berging-Ws}
  cErrorCreatingTable_Berging_FrOppWtr = -8056; {-fout bij het maken van tabel berging-Fractie oppervlaktewater}
  cErrorCreatingTable_Berging_Kwel     = -8057; {-fout bij het maken van tabel berging-kwel}

var
  Indx: Integer; {-Door de Boot-procedure moet de waarde van deze index worden ingevuld,
                   zodat de snelheidsprocedure 'weet' waar (op de externe parameter-array)
                   hij zijn gegevens moet zoeken}
  ModelProfile: TModelProfile;
                 {-Object met met daarin de status van de discontinuiteitsfuncties
                   (zie nDC) }

  {-Globally defined parameters from EP[0]}
  Table_Berging_Ws, Table_Ws_Berging,
  Table_Ws_Fr_Open_Wtr, Table_Ws_Kwel: TxyTableLinInt;

  {-Stuwparameters}
  Cv,                 {-Coefficient (-) [1.71]}
  cStuwBreedte,       {-Breedte stuw (m)}
  cKr,                {-Kruinhoogte (m)}
  cAfwOpp,            {-Afwaterend oppervlak (m2)}
  cH1Max,             {-Maximale overstorthoogte (m)}

  {-Wateraanvoer parameters}
  cHminWa,            {-Min.niveau wateraanvoer; als dit niveau onderschreden dan wordt w.a. gestart (m+NAP)}
  cHmaxWa: Double;    {-Max.niveau wateraanvoer; als dit niveau overschreden dan wordt w.a. weer gestopt (m+NAP)}

  {-Geldige range van key-/parameter/initiele waarden. De waarden van deze  variabelen moeten
    worden ingevuld door de Boot-procedure}
  cMin_NN,    cMax_NN,
  cMin_WaCap, cMax_WaCap,
  cMin_Ws,    cMax_Ws: Double;

Procedure MyDllProc( Reason: Integer );
begin
  if Reason = DLL_PROCESS_DETACH then begin {-DLL is unloading}
    {-Cleanup code here}
    if ( nDC > 0 ) then
      ModelProfile.Free;
    Try
      Table_Berging_Ws.Free;
      Table_Ws_Berging.Free;
      Table_Ws_Fr_Open_Wtr.free;
      Table_Ws_Kwel.free;
    Except
    end;
  end;
end;

Procedure DerivsProc( var x: Double; var y, dydx: TLargeRealArray;
                      var EP: TExtParArray; var Direction: TDirection;
                      var Context: Tcontext; var aModelProfile: PModelProfile; var IErr: Integer );
{-Deze procedure verschaft de array met afgeleiden 'dydx', gegeven het tijdstip 'x' en
  de toestand die beschreven wordt door de array 'y' en de externe condities die beschreven
  worden door de 'external parameter-array EP'. Als er geen fout op is getreden bij de
  berekening van 'dydx' dan wordt in deze procedure de variabele 'IErr' gelijk gemaakt aan
  de constante 'cNoError'. Opmerking: in de array 'y' staan dus de afhankelijke variabelen,
  terwijl 'x' de onafhankelijke variabele is (meestal de tijd)}
var
  {-Sleutel-waarden voor de default-tabellen in EP[cEP0]}
  {-}

  {-Parameter-waarden afkomstig van de Shell}
  NNgras,           {-Neerslagoverschot gras (m/d)}
  NNopenWater,      {Neerslagoverschot open water (m/d)}
  WaCap: Double;    {-wateraanvoercapaciteit (m/d)}

  {-Afgeleide (berekende) parameter-waarden}
  Ws,           {-waterstand (m+NAP)}
  Berging,      {-Berging in in oppervlaktewater, evt. vermeerderd met de berging in de onverzadigde zone (m)}
  Ov,           {-Afvoer door de stuw (m/d)}
  Kwel,         {-Toestroming naar berging via grondwater (kwel plus laterale toestroming; cumulatief, m)}
  Wateraanvoer, {-wateraanvoer (m/d)}
  Neerslagoverschot: {-Neerslagoverschot}
  Double;
  i: Integer;

Function GetWs( const Berging: Double ): Double; {-waterstand Ws bij Berging (m)}
begin
  Result := Table_Berging_Ws.EstimateY( Berging, FrWrd );
  {of:
    Result := EP[ cEP0 ].xInDep.Items[ cTb_Berging_Ws ].EstimateYLinInterpolation( Berging, 1, 2, iError );}
end;

Function SetParValuesFromEP0( var IErr: Integer ): Boolean;
  {-Fill globally defined parameters from EP[0]:
    Table_Berging_Ws, Table_Ws_Berging, Table_Ws_Fr_Open_Wtr, Table_Ws_Kwel & Stuwparameters}
const
  cColBerging = 1;
  cColWs1 = 1;
  cColWs2 = 2;
  cColFrOppWtr = 2;
  cColKwel = 2;
begin
  Result := True;
  IErr   := cNoError;
   {-Initialiseer de tabellen voor de beschrijving van de relatie tussen de
    berging (m) en de binnenwaterstand}
  Try {-Free first, in case this is not the first run}
    Table_Berging_Ws.free;
    Table_Ws_Berging.free;
  except
  end;
  Try
    Table_Berging_Ws := TxyTableLinInt.InitialiseFromDoubleMatrix( EP[ cEP0 ].xInDep.Items[ cTb_Berging_Ws ],
      cColBerging, cColWs2, IErr, nil );
    if ( IErr <> cNoError ) then
      Exception.Create( 'Error initialising Table_Berging_Ws.' );
    Table_Ws_Berging := TxyTableLinInt.InitialiseFromDoubleMatrix( EP[ cEP0 ].xInDep.Items[ cTb_Berging_Ws ],
      cColWs2, cColBerging, IErr, nil );
    if ( IErr <> cNoError ) then
      Exception.Create( 'Error initialising Table_Ws_Berging.' );
  Except
    IErr := cErrorCreatingTable_Berging_Ws;
    Result := False;
    Exit;
  end;
  with EP[ cEP0 ].xInDep.Items[ cTb_Berging_Ws ] do begin
    cMin_Ws := GetValue( 1, 2 );
    cMax_Ws := GetValue( GetNRows, 2 );
  end;

  Try {-Free first, in case this is not the first run}
    Table_Ws_Fr_Open_Wtr.Free;
  Except
  end;
  Try
    Table_Ws_Fr_Open_Wtr := TxyTableLinInt.InitialiseFromDoubleMatrix( EP[ cEP0 ].xInDep.Items[ cTb_Ws_FrOppWtr ],
       cColWs1, cColFrOppWtr, IErr, nil );
    if ( IErr <> cNoError ) then
      Exception.Create( 'Error initialising Table_Ws_Fr_Open_Wtr.' );
  Except
    IErr := cErrorCreatingTable_Berging_FrOppWtr;
    Result := False;
    Exit;
  end;

  Try {-Free first, in case this is not the first run}
    Table_Ws_Kwel.Free;
  Except
  end;
  Try
    Table_Ws_Kwel := TxyTableLinInt.InitialiseFromDoubleMatrix( EP[ cEP0 ].xInDep.Items[ cTb_Ws_Kwel ],
      cColWs1, cColKwel, IErr, nil );
  Except
    IErr := cErrorCreatingTable_Berging_Kwel;
    Result := False;
    Exit;
  end;

  {-Parameters voor de beschrijving van de stuw}
  with EP[ cEP0 ].xInDep.Items[ cTb_StuwPars ] do begin
    Cv                :=        GetValue( 1, 1 );
    cStuwBreedte      :=        GetValue( 1, 2 );
    cKr               :=        GetValue( 1, 3 );
    cAfwOpp           :=        GetValue( 1, 4 );
    cH1Max            :=        GetValue( 1, 5 );
  end;
  {-Parameters voor de beschrijving van de wateraanvoer}
  with EP[ cEP0 ].xInDep.Items[ cTb_WaPars ] do begin
    cHminWa := GetValue( 1, 1 );
    cHmaxWa := GetValue( 1, 2 );
  end;
end; {-Function SetParValuesFromEP0}

Function Replace_InitialValues_With_ShellValues( var IErr: Integer): Boolean;
  {-Als de Shell 1-of meer initiele waarden aanlevert voor de array met afhankelijke
    variabelen ('y'), dan kunnen deze waarden hier op deze array worden geplaatst en
    gecontroleerd}
var
  Ws_Init: Double;
begin
  IErr := cNoError; Result := True;
  with EP[ indx-1 ].xDep do
    Ws_Init := Items[ cTb_Ws_Init ].EstimateY( 0, Direction ); {Opm.: x=0}
  if ( Ws_Init < cMin_Ws ) or
     ( Ws_Init > cMax_Ws ) then begin
    IErr := cInvld_Ws; Result := False; Exit;
  end else begin {-Berging (m) bij waterstand Ws}
    y[ cBerging ] := Table_Ws_Berging.EstimateY( Ws_Init, FrWrd );
  end;
end; {-Replace_InitialValues_With_ShellValues}

Function SetKeyAndParValues( var IErr: Integer ): Boolean;
  Function GetParFromShell_NNgras( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_NNgras ].EstimateY( x, Direction );
  end;
  Function GetParFromShell_NNopenWater( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_NNopenWater ].EstimateY( x, Direction );
  end;
  Function GetParFromShell_WaCap( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_WaCap ].EstimateY( x, Direction );
  end;
begin {-Function SetKeyAndParValues}
  Result := False;
  NNgras := GetParFromShell_NNgras( x );
  if ( NNgras < cMin_NN ) or ( NNgras > cMax_NN ) then begin
    IErr := cInvld_NN; Exit;
  end;
  NNopenWater := GetParFromShell_NNopenWater( x );
  if ( NNopenWater < cMin_NN ) or ( NNopenWater > cMax_NN ) then begin
    IErr := cInvld_NN; Exit;
  end;
  WaCap := GetParFromShell_WaCap( x );
  if ( WaCap < cMin_WaCap ) or ( WaCap > cMax_WaCap ) then begin
    IErr := cInvld_WaCap; Exit;
  end;
  Result := True; IErr := cNoError;
end; {-Function SetKeyAndParValues}

Function Calc_Ov( const Ws: Double;          {-Waterstand (binnen) (m+NAP)}
                  var IErr: Integer ): Boolean;
var
  H1: Double; {-Overstorthoogte (m)}
const
  TinyH1 = 0.0005; {-Minimale overstorthoogte om tot afvoer te komen}
  NrOfSecondsInAday = 3600*24;
begin
  IErr   := cNoError;
  Ov     := 0;
  Result := True;
  H1 := Ws - cKr;
  if ( H1 <= TinyH1 ) then Exit;    {-Er is onvoldoende overstorthoogte aanwezig}
  if ( H1 > cH1Max ) then begin
    IErr := cH1Exceeded; Result := False; Exit;
  end;
  Ov := Cv * cStuwBreedte * Power( H1, 1.5 ) * NrOfSecondsInAday / cAfwOpp; {-m/d}
end; {-Function Calc_Ov}

Function Calc_Wateraanvoer( const Ws: Double; var IErr: Integer ): Boolean;
begin
  Result := True;
  IErr   := cNoError;
  if ( ( Ws < cHminWa ) and ( WaCap > 0 ) ) then
    Wateraanvoer := WaCap
  else
    Wateraanvoer := 0;
end;

Function Calc_Neerslagoverschot( const Ws: Double; var IErr: Integer ): Boolean;
var
  FractieOpenWater: Double;
begin
  Result := True;
  IErr   := cNoError;
  FractieOpenWater := Table_Ws_Fr_Open_Wtr.EstimateY( Ws, FrWrd );
  Neerslagoverschot := NNgras * ( 1 - FractieOpenWater ) + NNopenWater * FractieOpenWater;
end;

Function Calc_Kwel( const Ws: Double; var IErr: Integer ): Boolean;
begin
  Result := True;
  IErr   := cNoError;
  Kwel := Table_Ws_Kwel.EstimateY( Ws, FrWrd );
end;

begin
  IErr := cUnknownError;
  for i := 1 to cNrOfDepVar do {-Default speed = 0}
    dydx[ i ] := 0;

  {-Geef de aanroepende procedure een handvat naar het ModelProfiel}
  if ( nDC > 0 ) then
    aModelProfile := @ModelProfile
  else
    aModelProfile := NIL;

  if ( Context = UpdateYstart ) then begin {-Run fase 1}
    if not SetParValuesFromEP0( IErr ) then Exit;
    if not Replace_InitialValues_With_ShellValues( IErr )then Exit; {-Bepaal y[cBerging] op basis van Ws_Init}
    IErr := cNoError;
  end else begin {-Run fase 2}

    if not SetKeyAndParValues( IErr ) then {-NNgras, NNopenWater, WaCap}
      Exit;

    Berging := y[ cBerging ];   {-Berging in waterlopen en op maaiveld (m)}
    Ws      := GetWs( Berging ); {-Bijbehorende waterstand (m+NAP)}

    if ( Ws < cMin_Ws ) or ( Ws > cMax_Ws ) then begin
      IErr := cInvld_Ws; Exit;
    end;

    if not Calc_Ov( Ws, IErr ) then {-Afvoer over de stuw (m/d)}
        Exit;

    if not Calc_Wateraanvoer( Ws, IErr ) then
      Exit;

    if not Calc_Neerslagoverschot( Ws, IErr ) then
      Exit;

    if not Calc_Kwel( Ws, IErr ) then
      Exit;

    {-Bereken de array met afgeleiden 'dydx'.
	  Gebruik hierbij 'DCfunc' van 'ModelProfile' i.p.v.
	  'if'-statements! Als hierbij de 'AsSoonAs'-optie
	  wordt gebruikt, moet de statement worden aangevuld
	  met een extra conditie ( Context = Trigger ). Dus
	  bijv.: if DCfunc( AsSoonAs, h, LE, BodemNiveau, Context, cDCfunc0 )
	     and ( Context = Trigger ) then begin...}

    dydx[ cBerging ]     := Neerslagoverschot + Kwel - Ov + Wateraanvoer;
    dydx[ cCumWs ]       := Ws;
    dydx[ cCumOv ]       := Ov;
    dydx[ cCumKwel ]     := Kwel;
    dydx[ cCumQWa ]      := Wateraanvoer;
    dydx[ cCumNN ]       := Neerslagoverschot;
    dydx[ cHuidWaDuur ]  := 0;

  end;
end; {-DerivsProc}

Function DefaultBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Initialiseer de meest elementaire gegevens van het model. Shell-gegevens worden door deze
    procedure NIET verwerkt}
Procedure SetMinMaxKeyAndParValues;
begin
  with EP[ cEP0 ].xInDep.Items[ cTb_MinMaxValKeys ] do begin
    cMin_NN    := GetValue( 1, 1 ); {rij, kolom}
    cMax_NN    := GetValue( 1, 2 );
    cMin_WaCap := GetValue( 1, 3 );
    cMax_WaCap := GetValue( 1, 4 );
  end;
end;
Begin
  Result := DefaultBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cNrOfDepVar, nDC, cNrXIndepTblsInEP0,
                                       cNrXdepTblsInEP0, Indx, EP );
  if ( Result = cNoError ) then begin
    SetMinMaxKeyAndParValues;        {-Behalve cMin_Ws en cMax_Ws}
  end;
end;

Function TestBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Deze boot-procedure verwerkt alle basisgegevens van het model en leest de Shell-gegevens
    uit een bestand. Na initialisatie met deze boot-procedure is het model dus gereed om
	'te draaien'. Deze procedure kan dus worden gebruikt om het model 'los' van de Shell te
	testen}
Begin
  Result := DefaultBootEP( EpDir, BootEpArrayOption, EP );
  if ( Result <> cNoError ) then
    exit;
  Result := DefaultTestBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cnRP + cnSQ + cnRQ, Indx, EP );
  if ( Result <> cNoError ) then
    exit;
  SetReadyToRun( EP);
end;

Function BootEPForShell( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Deze procedure maakt het model gereed voor Shell-gebruik.
    De xDep-tables in EP[ indx-1 ] worden door deze procedure NIET geinitialiseerd omdat deze
	gegevens door de Shell worden verschaft }
begin
  Result := DefaultBootEP( EpDir, cBootEPFromTextFile, EP );
  if ( Result = cNoError ) then
    Result := DefaultBootEPForShell( cnRP, cnSQ, cnRQ, Indx, EP );
end;

Exports DerivsProc       index cModelIndxForTDSmodels, {999}
        DefaultBootEP    index cBoot0, {1}
        TestBootEP       index cBoot1, {2}
        BootEPForShell   index cBoot2; {3}

begin
  {-Dit zgn. 'DLL-Main-block' wordt uitgevoerd als de DLL voor het eerst in het geheugen wordt
    gezet (Reason = DLL_PROCESS_ATTACH)}
  DLLProc := @MyDllProc;
  Indx := cBootEPArrayVariantIndexUnknown;
  if ( nDC > 0 ) then
    ModelProfile := TModelProfile.Create( nDC );
end.
