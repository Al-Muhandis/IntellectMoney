// MockServer.pas - Mock HTTP Server for IntellectMoney API Testing
// Provides simulated API responses for unit testing without network access

unit MockServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpapp, fpwebfile, fphttp, httproute, httpprotocol, fpjson, fphttpserver, IntellectMoney
  ;

type
  // Тип обработчика запроса
  TRequestHandler = procedure(const APath: string; const AParams: string;
    var AResponse: string) of object;

  // Запись о запросе для проверок
  TRequestRecord = record
    Path: string;
    Method: string;
    Headers: TStringList;
    Body: string;
    Timestamp: TDateTime;
  end;
  PRequestRecord = ^TRequestRecord;

  TMockIntellectMoneyClient=class(TIntellectMoneyClient)
  public
    property ApiUrl;
  end;

  TMockHTTPServer= class(TFPHTTPServer)
  published
    property Address;
  end;

  { TMockServer }

  TMockServer = class(TThread)
  private
    FPort: Word;
    FRunning: Boolean;
    FServer: TMockHTTPServer;
    FRequestList: TList;
    FHandlers: TStringList; // Пары: URL = Response JSON
    FCriticalSection: TRTLCriticalSection;
    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    function GetResponse(const APath: string; const AParams: string): string;
    procedure RecordRequest(const APath, AMethod, ABody: string;
      AHeaders: TStringList);
  protected
    procedure Execute; override;
  public
    constructor Create(APort: Word = 8888);
    destructor Destroy; override;
    procedure ServerStart;
    procedure ServerStop;
    procedure RegisterHandler(const APath, AResponse: string);
    procedure ClearHandlers;
    procedure ClearRequests;
    function GetRequestCount: Integer;
    function GetLastRequest: TRequestRecord;
    function GetAllRequests: specialize TArray<TRequestRecord>;
    property IsRunning: Boolean read FRunning;
    property Port: Word read FPort write FPort;
  end;

  // Фабрика для создания тестовых ответов
  TResponseFactory = class
  public
    class function CreateInvoiceResponse(const AInvoiceId, AStatus: string;
      const AAmount: Double; const ACurrency: string): string;
    class function CreatePaymentStateResponse(const AState, ACode: string;
      const AAmount: Double): string;
    class function CreateBankCardPaymentResponse(const ACode, AMessage: string;
      ASuccess: Boolean): string;
    class function CreateErrorResponse(const AErrorCode, AErrorMessage: string): string;
  end;

implementation

{ TMockServer }

constructor TMockServer.Create(APort: Word = 8888);
begin
  inherited Create(True);
  FPort := APort;
  FRunning := False;
  FServer := nil;
  FRequestList := TList.Create;
  FHandlers := TStringList.Create;
  FHandlers.Sorted := True;
  InitCriticalSection(FCriticalSection);
  FreeOnTerminate := False;
end;

destructor TMockServer.Destroy;
var
  i: Integer;
begin
  ServerStop;

  EnterCriticalSection(FCriticalSection);
  try
    if Assigned(FRequestList) then
    begin
      for i := 0 to FRequestList.Count - 1 do
      begin
        if Assigned(PRequestRecord(FRequestList[i])^.Headers) then
          PRequestRecord(FRequestList[i])^.Headers.Free;
        Dispose(PRequestRecord(FRequestList[i]));
      end;
      FRequestList.Free;
    end;
    FHandlers.Free;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  DoneCriticalSection(FCriticalSection);
  inherited Destroy;
end;

procedure TMockServer.ServerStart;
begin
  if not FRunning then
  begin
    FRunning := True;
    Start;
  end;
end;

procedure TMockServer.ServerStop;
begin
  FRunning := False;
  if Assigned(FServer) then
    FServer.Active := False;

  if not Finished then
    WaitFor;
end;

procedure TMockServer.RegisterHandler(const APath, AResponse: string);
begin
  EnterCriticalSection(FCriticalSection);
  try
    FHandlers.Values[APath] := AResponse;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TMockServer.ClearHandlers;
begin
  EnterCriticalSection(FCriticalSection);
  try
    FHandlers.Clear;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TMockServer.ClearRequests;
var
  i: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    for i := 0 to FRequestList.Count - 1 do
    begin
      if Assigned(PRequestRecord(FRequestList[i])^.Headers) then
        PRequestRecord(FRequestList[i])^.Headers.Free;
      Dispose(PRequestRecord(FRequestList[i]));
    end;
    FRequestList.Clear;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TMockServer.GetRequestCount: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    Result := FRequestList.Count;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TMockServer.GetLastRequest: TRequestRecord;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if FRequestList.Count > 0 then
      Result := PRequestRecord(FRequestList[FRequestList.Count - 1])^
    else
      FillChar(Result, SizeOf(Result), 0);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TMockServer.GetAllRequests: specialize TArray<TRequestRecord>;
var
  i: Integer;
begin
  Result:=nil;
  EnterCriticalSection(FCriticalSection);
  try
    SetLength(Result, FRequestList.Count);
    for i := 0 to FRequestList.Count - 1 do
      Result[i] := PRequestRecord(FRequestList[i])^;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TMockServer.GetResponse(const APath: string; const AParams: string): string;
var
  idx: Integer;
begin
  Result := '';
  EnterCriticalSection(FCriticalSection);
  try
    idx := FHandlers.IndexOfName(APath);
    if idx >= 0 then
      Result := FHandlers.ValueFromIndex[idx];
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  // Если ответ не зарегистрирован, возвращаем стандартный успешный ответ
  if Result = '' then
    Result := '{"Result":{"Code":"Success","Desc":"OK"}}';
end;

procedure TMockServer.RecordRequest(const APath, AMethod, ABody: string;
  AHeaders: TStringList);
var
  Rec: PRequestRecord;
begin
  New(Rec);
  Rec^.Path := APath;
  Rec^.Method := AMethod;
  Rec^.Headers := TStringList.Create;
  if Assigned(AHeaders) then
    Rec^.Headers.Assign(AHeaders);
  Rec^.Body := ABody;
  Rec^.Timestamp := Now;

  EnterCriticalSection(FCriticalSection);
  try
    FRequestList.Add(Rec);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TMockServer.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Response: string;
  Headers: TStringList;
  i: Integer;
begin
  Headers := TStringList.Create;
  try
    // Копируем заголовки запроса
    for i := 0 to ARequest.CustomHeaders.Count - 1 do
      Headers.Add(ARequest.CustomHeaders[i]);

    // Записываем запрос
    RecordRequest(ARequest.URI, ARequest.Method, ARequest.Content, Headers);

    // Получаем ответ
    Response := GetResponse(ARequest.URI, ARequest.Content);

    // Настраиваем ответ
    AResponse.Content := Response;
    AResponse.ContentType := 'application/json';
    AResponse.ContentLength := Length(Response);
    AResponse.Code := 200;
    AResponse.CodeText := 'OK';

  finally
    Headers.Free;
  end;
end;

procedure TMockServer.Execute;
begin
  try
    FServer := TMockHTTPServer.Create(nil);
    try
      FServer.Port := FPort;
      FServer.Address := '127.0.0.1';
      FServer.Threaded := True;
      FServer.OnRequest := @HandleRequest;
      FServer.AcceptIdleTimeout := 1000;
      FServer.Active := True;

      while FRunning and FServer.Active do
      begin
        Sleep(100);
        CheckSynchronize(100);
      end;

    finally
      FServer.Active := False;
      FreeAndNil(FServer);
    end;
  except
    on E: Exception do
    begin
      // Логируем ошибку, если необходимо
    end;
  end;
end;

{ TResponseFactory }

class function TResponseFactory.CreateInvoiceResponse(const AInvoiceId,
  AStatus: string; const AAmount: Double; const ACurrency: string): string;
var
  JsonObj: TJSONObject;
  ResultObj: TJSONObject;
  aOperationStateObj: TJSONObject;
  aStateObj: TJSONObject;
begin
  JsonObj := TJSONObject.Create;
  aOperationStateObj := TJSONObject.Create;  
  aStateObj := TJSONObject.Create;
  ResultObj := TJSONObject.Create;

  try
    aOperationStateObj.Add('Code', 0);
    aOperationStateObj.Add('Desc', 'OK'); 
    JsonObj.Add('OperationState', aOperationStateObj);

    aStateObj.Add('Code', 0);
    aStateObj.Add('Desc', AStatus); 
    ResultObj.Add('State', aStateObj);
    ResultObj.Add('InvoiceId', AInvoiceId);

    JsonObj.Add('Result', ResultObj);

    Result := JsonObj.AsJSON;
  finally
    JsonObj.Free;
  end;
end;

class function TResponseFactory.CreatePaymentStateResponse(const AState,
  ACode: string; const AAmount: Double): string;
var
  JsonObj: TJSONObject;
  ResultObj: TJSONObject;
  StateObj: TJSONObject;
begin
  JsonObj := TJSONObject.Create;
  StateObj := TJSONObject.Create;
  ResultObj := TJSONObject.Create;

  try
    StateObj.Add('Code', 'Success');
    StateObj.Add('Desc', 'OK');

    ResultObj.Add('Code', ACode);
    ResultObj.Add('PaymentStep', AState);
    ResultObj.Add('Amount', FormatFloat('0.00', AAmount));

    JsonObj.Add('OperationState', StateObj);
    JsonObj.Add('Result', ResultObj);

    Result := JsonObj.AsJSON;
  finally
    JsonObj.Free;
  end;
end;

class function TResponseFactory.CreateBankCardPaymentResponse(
  const ACode, AMessage: string; ASuccess: Boolean): string;
var
  JsonObj: TJSONObject;
  ResultObj: TJSONObject;
  StateObj: TJSONObject;
begin
  JsonObj := TJSONObject.Create;
  StateObj := TJSONObject.Create;
  ResultObj := TJSONObject.Create;

  try
    StateObj.Add('Code', 'Success');
    StateObj.Add('Desc', 'OK');

    if ASuccess then
      ResultObj.Add('Code', 'Success')
    else
      ResultObj.Add('Code', 'Failed');

    ResultObj.Add('Desc', AMessage);

    JsonObj.Add('OperationState', StateObj);
    JsonObj.Add('Result', ResultObj);

    Result := JsonObj.AsJSON;
  finally
    JsonObj.Free;
  end;
end;

class function TResponseFactory.CreateErrorResponse(const AErrorCode,
  AErrorMessage: string): string;
var
  JsonObj: TJSONObject;
  StateObj: TJSONObject;
  ResultObj: TJSONObject;
begin
  JsonObj := TJSONObject.Create;
  StateObj := TJSONObject.Create;
  ResultObj := TJSONObject.Create;

  try
    StateObj.Add('Code', AErrorCode);
    StateObj.Add('Desc', AErrorMessage);

    ResultObj.Add('Code', AErrorCode);
    ResultObj.Add('Desc', AErrorMessage);

    JsonObj.Add('OperationState', StateObj);
    JsonObj.Add('Result', ResultObj);

    Result := JsonObj.AsJSON;
  finally
    JsonObj.Free;
  end;
end;

end.
