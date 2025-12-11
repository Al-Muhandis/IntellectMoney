// IntellectMoney.pas - API Wrapper for FreePascal/Lazarus
// Документация: https://wiki.intellectmoney.ru/pages/viewpage.action?pageId=4849803

unit intellectmoney_merchant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, intellectmoney_base
  ;

type
  TReceiptPosition = record
    Quantity: Double;
    Price: Double;
    Tax: Integer;                    // 1-6 (see doc)
    Text: string;
    PaymentSubjectType: Integer;     // 1-13 (optional)
    PaymentMethodType: Integer;      // 1-7 (optional)
    SupplierINN: string;             // up to 12 characters (optional)
  end;

  TReceiptPayment = record
    PaymentType: Integer;            // 1, 2, 14, 15, 16
    Amount: Double;
  end;

  TPaymentsArray = array of TReceiptPayment;
  TPositionsArray = array of TReceiptPosition;

  { TIntellectMoneyMerchantClient }
  TIntellectMoneyMerchantClient = class(TIntellectMoneyBaseClient)
  private
    FOrderId: string;
    FAmount: Double;
    FCurrency: string;
    FEmail: string;
    FUserName: string;
    FServiceName: string;
    FSuccessUrl: string;
    FBackUrl: string;

    // Receipt fields
    FINN: string;
    FGroup: string;
    FPositions: TPositionsArray;
    FPayments: TPaymentsArray;
    FTaxationSystem: Integer;
    FSkipAmountCheck: Integer;

    function GenerateHash(const aOrderId, aServiceName, aAmount, aCurrency: string): String;
    function BuildMerchantReceipt(const aINN, aGroup, aCustomerContact: string;
      const aPositions: array of TReceiptPosition;
      aTaxationSystem: Integer = 0;
      const aPayments: TPaymentsArray = nil;
      aSkipAmountCheck: Integer = 0): string;
    function BuildPaymentParams(
      const aOrderId: string;
      const aAmount: Double;
      const aCurrency: string;
      const aEmail: string;
      const aUserName: string;
      const aServiceName: string;
      const aSuccessUrl: string;
      const aBackURL: string;
      const aMerchantReceipt: string = ''
    ): TStringList;
  public
    constructor Create(const aEshopId, aSecretKey: string); override;
    function CreatePaymentURL(
      const aOrderId: string;
      const aAmount: Double;
      const aCurrency: string;
      const aEmail: string = '';
      const aUserName: string = '';
      const aServiceName: string = '';
      const aSuccessUrl: string = '';
      const aBackURL: string = ''
    ): string;
    function CreatePaymentURLWithReceipt(
      const aOrderId: string;
      const aAmount: Double;
      const aCurrency: string;
      const aEmail: string;
      const aINN: string;
      const aGroup: string;
      const aPositions: array of TReceiptPosition;
      aTaxationSystem: Integer = 0;
      const aUserName: string = '';
      const aServiceName: string = '';
      const aSuccessUrl: string = '';
      const aBackURL: string = '';
      const aPayments: TPaymentsArray = nil;
      aSkipAmountCheck: Integer = 0
    ): string;

    function BuildURL: string;
    function BuildURLWithReceipt: string;

    { Works with receipt positions }
    procedure AddPosition(const aPosition: TReceiptPosition);
    procedure ClearPositions;
    function GetPositionCount: Integer;

    { Works with payments }
    procedure AddPayment(const aPayment: TReceiptPayment);
    procedure ClearPayments;

    procedure Reset;

    { Main properties of a payment }
    property OrderId: string read FOrderId write FOrderId;
    property Amount: Double read FAmount write FAmount;
    property Currency: string read FCurrency write FCurrency;
    property Email: string read FEmail write FEmail;
    property UserName: string read FUserName write FUserName;
    property ServiceName: string read FServiceName write FServiceName;
    property SuccessUrl: string read FSuccessUrl write FSuccessUrl;
    property BackUrl: string read FBackUrl write FBackUrl;

    { Properties for online sales register }
    property INN: string read FINN write FINN;
    property Group: string read FGroup write FGroup;
    property TaxationSystem: Integer read FTaxationSystem write FTaxationSystem;
    property SkipAmountCheck: Integer read FSkipAmountCheck write FSkipAmountCheck;

    property Positions: TPositionsArray read FPositions write FPositions;
    property Payments: TPaymentsArray read FPayments write FPayments;
  end;

implementation

uses
  md5, fphttpclient, jsonscanner, fpjson;

var
  _FrmtStngsAPI: TFormatSettings;

function EncodeURLParams(aParams: TStringList): string;
var
  i: Integer;
  aEncoded: TStringList;
begin
  aEncoded := TStringList.Create;
  aEncoded.Delimiter := '&';
  try
    for i := 0 to aParams.Count - 1 do
      aEncoded.AddPair(aParams.Names[i], EncodeURLElement(aParams.ValueFromIndex[i]));
    Result := aEncoded.DelimitedText;
  finally
    aEncoded.Free;
  end;
end;

{ TIntellectMoneyMerchantClient }

function TIntellectMoneyMerchantClient.BuildMerchantReceipt(
  const aINN, aGroup, aCustomerContact: string;
  const aPositions: array of TReceiptPosition;
  aTaxationSystem: Integer;
  const aPayments: TPaymentsArray;
  aSkipAmountCheck: Integer): string;
var
  Receipt, Content, CheckClose: TJSONObject;
  PositionsArray, PaymentsArray: TJSONArray;
  Position, Payment: TJSONObject;
  i: Integer;
begin
  Receipt := TJSONObject.Create;
  try
    // Добавляем основные параметры
    if aSkipAmountCheck <> 0 then
      Receipt.Add('skipAmountCheck', aSkipAmountCheck);

    Receipt.Add('inn', aINN);
    Receipt.Add('group', aGroup);

    // Создаем content
    Content := TJSONObject.Create;
    Content.Add('type', 1);  // Тип документа: 1 = приход
    Content.Add('customerContact', aCustomerContact);

    // Добавляем позиции
    PositionsArray := TJSONArray.Create;
    for i := Low(aPositions) to High(aPositions) do
    begin
      Position := TJSONObject.Create;
      Position.Add('quantity', aPositions[i].Quantity);
      Position.Add('price', aPositions[i].Price);
      Position.Add('tax', aPositions[i].Tax);
      Position.Add('text', aPositions[i].Text);

      if aPositions[i].PaymentSubjectType > 0 then
        Position.Add('paymentSubjectType', aPositions[i].PaymentSubjectType);

      if aPositions[i].PaymentMethodType > 0 then
        Position.Add('paymentMethodType', aPositions[i].PaymentMethodType);

      if aPositions[i].SupplierINN <> '' then
        Position.Add('supplierINN', aPositions[i].SupplierINN);

      PositionsArray.Add(Position);
    end;
    Content.Add('positions', PositionsArray);

    Receipt.Add('content', Content);

    // Добавляем checkClose с платежами (если используется API)
    if Length(aPayments) > 0 then
    begin
      CheckClose := TJSONObject.Create;

      PaymentsArray := TJSONArray.Create;
      for i := Low(aPayments) to High(aPayments) do
      begin
        Payment := TJSONObject.Create;
        Payment.Add('type', aPayments[i].PaymentType);
        Payment.Add('amount', aPayments[i].Amount);
        PaymentsArray.Add(Payment);
      end;
      CheckClose.Add('payments', PaymentsArray);
      CheckClose.Add('taxationSystem', aTaxationSystem);

      Receipt.Add('checkClose', CheckClose);
    end;

    Result := Receipt.AsJSON;
  finally
    Receipt.Free;
  end;
end;

function TIntellectMoneyMerchantClient.GenerateHash(
  const aOrderId, aServiceName, aAmount, aCurrency: string): String;
var
  aSignString: string;
begin
  aSignString := EshopId + '::' + aOrderId + '::' + aServiceName + '::' +
                 aAmount + '::' + aCurrency + '::' + SecretKey;
  Result := MD5Print(MD5String(aSignString));
end;

function TIntellectMoneyMerchantClient.BuildPaymentParams(
  const aOrderId: string;
  const aAmount: Double;
  const aCurrency: string;
  const aEmail: string;
  const aUserName: string;
  const aServiceName: string;
  const aSuccessUrl: string;
  const aBackURL: string;
  const aMerchantReceipt: string = ''
): TStringList;
var
  aAmountStr, aHash: string;
begin
  Result := TStringList.Create;

  aAmountStr := FormatFloat('0.00', aAmount, _FrmtStngsAPI);

  Result.Values['eshopId'] := EshopId;
  Result.Values['orderId'] := aOrderId;
  Result.Values['serviceName'] := aServiceName;
  Result.Values['recipientAmount'] := aAmountStr;
  Result.Values['recipientCurrency'] := aCurrency;
  Result.Values['userName'] := aUserName;
  Result.Values['user_email'] := aEmail;
  Result.Values['successUrl'] := aSuccessUrl;
  Result.Values['backUrl'] := aBackURL;

  // Добавляем чек, если он передан
  if aMerchantReceipt <> '' then
    Result.Values['merchantReceipt'] := aMerchantReceipt;

  // Генерируем хеш
  aHash := GenerateHash(
    Result.Values['orderId'],
    Result.Values['serviceName'],
    Result.Values['recipientAmount'],
    Result.Values['recipientCurrency']
  );
  Result.Values['hash'] := aHash;
end;

constructor TIntellectMoneyMerchantClient.Create(const aEshopId, aSecretKey: string);
begin
  inherited Create(aEshopId, aSecretKey);
  Url := 'https://merchant.intellectmoney.ru/%s/';
  Reset;
end;

function TIntellectMoneyMerchantClient.CreatePaymentURL(
  const aOrderId: string;
  const aAmount: Double;
  const aCurrency: string;
  const aEmail: string;
  const aUserName: string;
  const aServiceName: string;
  const aSuccessUrl: string;
  const aBackURL: string
): string;
var
  aParams: TStringList;
begin
  aParams := BuildPaymentParams(
    aOrderId, aAmount, aCurrency, aEmail, aUserName,
    aServiceName, aSuccessUrl, aBackURL
  );
  try
    Result := Format(Url, [Lang]) + '?' + EncodeURLParams(aParams);
  finally
    aParams.Free;
  end;
end;

function TIntellectMoneyMerchantClient.CreatePaymentURLWithReceipt(
  const aOrderId: string;
  const aAmount: Double;
  const aCurrency: string;
  const aEmail: string;
  const aINN: string;
  const aGroup: string;
  const aPositions: array of TReceiptPosition;
  aTaxationSystem: Integer;
  const aUserName: string;
  const aServiceName: string;
  const aSuccessUrl: string;
  const aBackURL: string;
  const aPayments: TPaymentsArray;
  aSkipAmountCheck: Integer
): string;
var
  aParams: TStringList;
  aMerchantReceipt: string;
begin
  // Формируем чек
  aMerchantReceipt := BuildMerchantReceipt(
    aINN, aGroup, aEmail, aPositions, aTaxationSystem, aPayments, aSkipAmountCheck
  );

  aParams := BuildPaymentParams(
    aOrderId, aAmount, aCurrency, aEmail, aUserName,
    aServiceName, aSuccessUrl, aBackURL, aMerchantReceipt
  );
  try
    Result := Format(Url, [Lang]) + '?' + EncodeURLParams(aParams);
  finally
    aParams.Free;
  end;
end;

{ Методы для работы через свойства }

function TIntellectMoneyMerchantClient.BuildURL: string;
var
  aParams: TStringList;
begin
  aParams := BuildPaymentParams(
    FOrderId, FAmount, FCurrency, FEmail, FUserName,
    FServiceName, FSuccessUrl, FBackUrl
  );
  try
    Result := Format(Url, [Lang]) + '?' + EncodeURLParams(aParams);
  finally
    aParams.Free;
  end;
end;

function TIntellectMoneyMerchantClient.BuildURLWithReceipt: string;
var
  aParams: TStringList;
  aMerchantReceipt: string;
begin
  // Формируем чек
  aMerchantReceipt := BuildMerchantReceipt(
    FINN, FGroup, FEmail, FPositions, FTaxationSystem, FPayments, FSkipAmountCheck
  );

  aParams := BuildPaymentParams(
    FOrderId, FAmount, FCurrency, FEmail, FUserName,
    FServiceName, FSuccessUrl, FBackUrl, aMerchantReceipt
  );
  try
    Result := Format(Url, [Lang]) + '?' + EncodeURLParams(aParams);
  finally
    aParams.Free;
  end;
end;

procedure TIntellectMoneyMerchantClient.AddPosition(const aPosition: TReceiptPosition);
var
  Len: Integer;
begin
  Len := Length(FPositions);
  SetLength(FPositions, Len + 1);
  FPositions[Len] := aPosition;
end;

procedure TIntellectMoneyMerchantClient.ClearPositions;
begin
  SetLength(FPositions, 0);
end;

function TIntellectMoneyMerchantClient.GetPositionCount: Integer;
begin
  Result := Length(FPositions);
end;

procedure TIntellectMoneyMerchantClient.AddPayment(const aPayment: TReceiptPayment);
var
  Len: Integer;
begin
  Len := Length(FPayments);
  SetLength(FPayments, Len + 1);
  FPayments[Len] := aPayment;
end;

procedure TIntellectMoneyMerchantClient.ClearPayments;
begin
  SetLength(FPayments, 0);
end;

procedure TIntellectMoneyMerchantClient.Reset;
begin
  FOrderId := '';
  FAmount := 0;
  FCurrency := 'RUB';
  FEmail := '';
  FUserName := '';
  FServiceName := '';
  FSuccessUrl := '';
  FBackUrl := '';

  FINN := '';
  FGroup := '';
  FTaxationSystem := 0;
  FSkipAmountCheck := 0;

  SetLength(FPositions, 0);
  SetLength(FPayments, 0);
end;

initialization
  _FrmtStngsAPI := DefaultFormatSettings;
  _FrmtStngsAPI.DecimalSeparator := '.';

end.

