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
    Tax: Integer;        // 1-6 (see doc)
    Text: string;
    PaymentSubjectType: Integer;  // 1-13 (optional)
    PaymentMethodType: Integer;   // 1-7 (optional)
    SupplierINN: string;          // up to 12 characters (optional)
  end;

  TReceiptPayment = record
    PaymentType: Integer;  // 1, 2, 14, 15, 16
    Amount: Double;
  end;

  TPaymentsArray = array of TReceiptPayment;

  { TIntellectMoneyMerchantClient }

  TIntellectMoneyMerchantClient = class(TIntellectMoneyBaseClient)
  private
    function GenerateHash(const aOrderId, aServiceName, aAmount, aCurrency: string): String;
    function BuildMerchantReceipt(const aINN, aGroup, aCustomerContact: string;
      const aPositions: array of TReceiptPosition;
      aTaxationSystem: Integer = 0;
      const aPayments: TPaymentsArray = nil;
      aSkipAmountCheck: Integer = 0): string;
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
      const aFailUrl: string = '';
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
      const aFailUrl: string = '';
      const aBackURL: string = '';
      const aPayments: TPaymentsArray = nil;
      aSkipAmountCheck: Integer = 0
    ): string;
  end;

implementation

uses
  md5, fphttpclient, jsonscanner, fpjson
  ;

var
  _FrmtStngsAPI: TFormatSettings;

function EncodeURLParams(aParams: TStringList): string;
var
  i: Integer;
  aEncoded: TStringList;
begin
  aEncoded := TStringList.Create;
  aEncoded.Delimiter:='&';
  try
    for i := 0 to aParams.Count - 1 do
      aEncoded.AddPair(aParams.Names[i], EncodeURLElement(aParams.ValueFromIndex[i]));
    Result := aEncoded.DelimitedText;
  finally
    aEncoded.Free;
  end;
end;

{ TIntellectMoneyMerchantClient }

function TIntellectMoneyMerchantClient.BuildMerchantReceipt(const aINN, aGroup, aCustomerContact: string;
  const aPositions: array of TReceiptPosition; aTaxationSystem: Integer; const aPayments: TPaymentsArray;
  aSkipAmountCheck: Integer): string;
var
  Receipt, Content, CheckClose: TJSONObject;
  Positions, Payments: TJSONArray;
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
    Positions := TJSONArray.Create;
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

      Positions.Add(Position);
    end;
    Content.Add('positions', Positions);

    Receipt.Add('content', Content);

    // Добавляем checkClose с платежами (если используется API)
    if Length(aPayments) > 0 then
    begin
      CheckClose := TJSONObject.Create;

      Payments := TJSONArray.Create;
      for i := Low(aPayments) to High(aPayments) do
      begin
        Payment := TJSONObject.Create;
        Payment.Add('type', aPayments[i].PaymentType);
        Payment.Add('amount', aPayments[i].Amount);
        Payments.Add(Payment);
      end;
      CheckClose.Add('payments', Payments);
      CheckClose.Add('taxationSystem', aTaxationSystem);

      Receipt.Add('checkClose', CheckClose);
    end;

    Result := Receipt.AsJSON;
  finally
    Receipt.Free;
  end;
end;

function TIntellectMoneyMerchantClient.GenerateHash(const aOrderId, aServiceName, aAmount, aCurrency: string): String;
var
  aSignString: string;
begin
  aSignString := EshopId+'::'+aOrderId+'::'+aServiceName+'::'+aAmount+'::'+aCurrency+'::'+SecretKey;
  Result := MD5Print(MD5String(aSignString));
end;

constructor TIntellectMoneyMerchantClient.Create(const aEshopId, aSecretKey: string);
begin
  inherited Create(aEshopId, aSecretKey);
  Url:='https://merchant.intellectmoney.ru/%s/';
end;

function TIntellectMoneyMerchantClient.CreatePaymentURL(const aOrderId: string; const aAmount: Double;
  const aCurrency: string; const aEmail: string; const aUserName: string; const aServiceName: string;
  const aSuccessUrl: string; const aFailUrl: string; const aBackURL: string): string;
var
  aParams: TStringList;
  aHash, aAmountStr: string;
begin
  aParams := TStringList.Create;
  try
    aAmountStr := FormatFloat('0.00', AAmount, _FrmtStngsAPI);

    aParams.Values['eshopId'] :=           EshopId;
    aParams.Values['orderId'] :=           aOrderId;
    aParams.Values['serviceName'] :=       aServiceName;
    aParams.Values['recipientAmount'] :=   aAmountStr;
    aParams.Values['recipientCurrency'] := aCurrency;
    aParams.Values['userName'] :=          aUserName;
    aParams.Values['userEmail'] :=         aEmail;
    aParams.Values['successUrl'] :=        aSuccessUrl;
    aParams.Values['failUrl'] :=           aFailUrl;
    aParams.Values['backUrl'] :=           aBackURL;

    aHash := GenerateHash(
      aParams.Values['orderId'],
      aParams.Values['serviceName'],
      aParams.Values['recipientAmount'],
      aParams.Values['recipientCurrency']
    );
    aParams.Values['hash'] := aHash;

    Result := Format(Url, [Lang]) +'?'+EncodeURLParams(aParams);
  finally
    aParams.Free;
  end;
end;

function TIntellectMoneyMerchantClient.CreatePaymentURLWithReceipt(const aOrderId: string; const aAmount: Double;
  const aCurrency: string; const aEmail: string; const aINN: string; const aGroup: string;
  const aPositions: array of TReceiptPosition; aTaxationSystem: Integer; const aUserName: string;
  const aServiceName: string; const aSuccessUrl: string; const aFailUrl: string; const aBackURL: string;
  const aPayments: TPaymentsArray; aSkipAmountCheck: Integer): string;
var
  aParams: TStringList;
  aHash, aAmountStr, aMerchantReceipt: string;
begin
  aParams := TStringList.Create;
  try
    aAmountStr := FormatFloat('0.00', AAmount, _FrmtStngsAPI);

    // Формируем чек
    aMerchantReceipt := BuildMerchantReceipt(
      aINN, aGroup, aEmail, aPositions, aTaxationSystem, aPayments, aSkipAmountCheck
    );

    aParams.Values['eshopId'] := EshopId;
    aParams.Values['orderId'] := aOrderId;
    aParams.Values['serviceName'] := aServiceName;
    aParams.Values['recipientAmount'] := aAmountStr;
    aParams.Values['recipientCurrency'] := aCurrency;
    aParams.Values['userName'] := aUserName;
    aParams.Values['user_email'] := aEmail;
    aParams.Values['successUrl'] := aSuccessUrl;
    aParams.Values['failUrl'] := aFailUrl;
    aParams.Values['backUrl'] := aBackURL;
    aParams.Values['merchantReceipt'] := aMerchantReceipt;

    aHash := GenerateHash(
      aParams.Values['orderId'],
      aParams.Values['serviceName'],
      aParams.Values['recipientAmount'],
      aParams.Values['recipientCurrency']
    );
    aParams.Values['hash'] := aHash;

    Result := Format('%s%s/?%s', [Url, Lang, EncodeURLParams(aParams)]);
  finally
    aParams.Free;
  end;
end;

initialization
  _FrmtStngsAPI := DefaultFormatSettings;
  _FrmtStngsAPI.DecimalSeparator := '.';

end.
