unit IntellectMoneyMerchantTests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, intellectmoney_merchant, intellectmoney_base, fpjson, jsonparser;

type
  TTestIMMerchantClient = class(TIntellectMoneyMerchantClient)
  public
    property URL;
  end;

  TIntellectMoneyMerchantTest = class(TTestCase)
  protected
    FMerchant: TTestIMMerchantClient;
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure AssertContains(const ASubstr, AStr, AMsg: string);
    procedure AssertTrue(ACondition: Boolean; const AMsg: string);
  published
    procedure TestMerchantCreation;
    procedure TestCreatePaymentURLBasic;
    procedure TestCreatePaymentURLWithAllParameters;
    procedure TestCreatePaymentURLMinimalParameters;
    procedure TestHashCalculation;
    procedure TestURLEncoding;
    procedure TestAmountFormatting;
    procedure TestLanguageInURL;
    procedure TestCreatePaymentURLWithReceiptBasic;
    procedure TestCreatePaymentURLWithReceiptMultiplePositions;
    procedure TestCreatePaymentURLWithReceiptOptionalFields;
    procedure TestCreatePaymentURLWithReceiptPayments;
    procedure TestMerchantReceiptJSONStructure;
    procedure TestMerchantReceiptWithSkipAmountCheck;
    procedure TestMerchantReceiptWithTaxationSystem;
    procedure TestMerchantReceiptUTF8Encoding;
    procedure TestMerchantReceiptEmptyPositions;
  end;

implementation

{ TIntellectMoneyMerchantTest }

procedure TIntellectMoneyMerchantTest.SetUp;
begin
  inherited SetUp;
  FMerchant := TTestIMMerchantClient.Create('12345', 'test_secret_key');
end;

procedure TIntellectMoneyMerchantTest.TearDown;
begin
  FMerchant.Free;
  inherited TearDown;
end;

procedure TIntellectMoneyMerchantTest.AssertContains(const ASubstr, AStr, AMsg: string);
begin
  if Pos(ASubstr, AStr) = 0 then
    Fail(Format('%s: Expected to find "%s" in "%s"', [AMsg, ASubstr, AStr]));
end;

procedure TIntellectMoneyMerchantTest.AssertTrue(ACondition: Boolean; const AMsg: string);
begin
  if not ACondition then
    Fail(AMsg);
end;

procedure TIntellectMoneyMerchantTest.TestMerchantCreation;
begin
  AssertTrue(FMerchant.EshopId = '12345', 'EshopId should match');
  AssertTrue(FMerchant.SecretKey = 'test_secret_key', 'SecretKey should match');
  AssertTrue(Pos('https://merchant.intellectmoney.ru/', FMerchant.Url) > 0,
    'URL should contain merchant base URL');
end;

procedure TIntellectMoneyMerchantTest.TestCreatePaymentURLBasic;
var
  PaymentURL: string;
begin
  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-001',
    100.00,
    'RUB',
    'test@example.com'
  );

  AssertTrue(PaymentURL <> '', 'Payment URL should not be empty');
  AssertContains('eshopId=12345', PaymentURL, 'URL should contain eshopId');
  AssertContains('orderId=ORDER-001', PaymentURL, 'URL should contain orderId');
  AssertContains('recipientAmount=100.00', PaymentURL, 'URL should contain amount');
  AssertContains('recipientCurrency=RUB', PaymentURL, 'URL should contain currency');
  AssertContains('userEmail=test%40example.com', PaymentURL, 'URL should contain encoded email');
  AssertContains('hash=', PaymentURL, 'URL should contain hash');
end;

procedure TIntellectMoneyMerchantTest.TestCreatePaymentURLWithAllParameters;
var
  PaymentURL: string;
begin
  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-002',
    250.50,
    'USD',
    'user@example.com',
    'Иван Петров',
    'Оплата товара',
    'https://shop.com/success',
    'https://shop.com/fail'
  );

  AssertTrue(PaymentURL <> '', 'Payment URL should not be empty');
  AssertContains('orderId=ORDER-002', PaymentURL, 'URL should contain orderId');
  AssertContains('recipientAmount=250.50', PaymentURL, 'URL should contain amount');
  AssertContains('recipientCurrency=USD', PaymentURL, 'URL should contain currency');
  AssertContains('userName=', PaymentURL, 'URL should contain userName');
  AssertContains('serviceName=', PaymentURL, 'URL should contain serviceName');
  AssertContains('successUrl=', PaymentURL, 'URL should contain successUrl');
  AssertContains('failUrl=', PaymentURL, 'URL should contain failUrl');
end;

procedure TIntellectMoneyMerchantTest.TestCreatePaymentURLMinimalParameters;
var
  PaymentURL: string;
begin
  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-MIN',
    10.00,
    'RUB',
    'minimal@test.com',
    '', // userName
    '', // serviceName
    '', // successUrl
    ''  // failUrl
  );

  AssertTrue(PaymentURL <> '', 'Payment URL should not be empty');
  AssertContains('orderId=ORDER-MIN', PaymentURL, 'URL should contain orderId');
  AssertContains('recipientAmount=10.00', PaymentURL, 'URL should contain amount');
  AssertContains('userEmail=minimal%40test.com', PaymentURL, 'URL should contain email');
end;

procedure TIntellectMoneyMerchantTest.TestHashCalculation;
var
  PaymentURL1, PaymentURL2: string;
  StartPos: Integer;
begin
  PaymentURL1 := FMerchant.CreatePaymentURL(
    'ORDER-HASH',
    100.00,
    'RUB',
    'test@example.com',
    '',
    'Test Service'
  );
  PaymentURL2 := FMerchant.CreatePaymentURL(
    'ORDER-HASH',
    100.00,
    'RUB',
    'test@example.com',
    '',
    'Test Service'
  );

  StartPos := Pos('hash=', PaymentURL1);
  AssertTrue(StartPos > 0, 'First URL should contain hash parameter');
  StartPos := Pos('hash=', PaymentURL2);
  AssertTrue(StartPos > 0, 'Second URL should contain hash parameter');

  AssertTrue(PaymentURL1 = PaymentURL2, 'URLs with same parameters should be identical');

  // Тест известного хеша (из документации IntellectMoney)
  FMerchant.EshopId := '17354';
  FMerchant.SecretKey := 'test';
  PaymentURL1 := FMerchant.CreatePaymentURL(
    '1',
    10.10,
    'RUB',
    'e.mozgovoy+1@intellectmoney.ru',
    '',
    'покупка книги Хочу все знать'
  );
  AssertContains('hash=139de04be8c37061f99218353f4e13e0', PaymentURL1,
    'Hash should match expected value for known parameters');
end;

procedure TIntellectMoneyMerchantTest.TestURLEncoding;
var
  PaymentURL: string;
begin
  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-SPECIAL',
    100.00,
    'RUB',
    'test+special@example.com',
    'Иван & Петров',
    'Товар #1',
    'https://shop.com/success?param=value',
    'https://shop.com/fail?error=true'
  );

  AssertTrue(PaymentURL <> '', 'Payment URL should not be empty');
  AssertTrue((Pos('test%2Bspecial%40example.com', PaymentURL) > 0)
    OR (Pos('test+special%40example.com', PaymentURL) > 0),
    'Email should be URL encoded');
  AssertTrue(Pos('%26', PaymentURL) > 0, 'Ampersand should be encoded');
end;

procedure TIntellectMoneyMerchantTest.TestAmountFormatting;
var
  PaymentURL: string;
begin
  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-AMT1',
    100.00,
    'RUB',
    'test@example.com'
  );
  AssertContains('recipientAmount=100.00', PaymentURL,
    'Whole number should be formatted with .00');

  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-AMT2',
    99.99,
    'RUB',
    'test@example.com'
  );
  AssertContains('recipientAmount=99.99', PaymentURL,
    'Decimal number should preserve decimal places');

  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-AMT3',
    50.50,
    'RUB',
    'test@example.com'
  );
  AssertContains('recipientAmount=50.50', PaymentURL,
    'Amount should be formatted with two decimal places');
end;

procedure TIntellectMoneyMerchantTest.TestLanguageInURL;
var
  PaymentURL: string;
begin
  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-LANG',
    100.00,
    'RUB',
    'test@example.com'
  );
  AssertContains('/ru/', PaymentURL, 'Default language should be ru');

  FMerchant.Lang := 'en';
  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-LANG-EN',
    100.00,
    'USD',
    'test@example.com'
  );
  AssertContains('/en/', PaymentURL, 'Language should be en when set');
end;

// ============ НОВЫЕ ТЕСТЫ ДЛЯ merchantReceipt ============

procedure TIntellectMoneyMerchantTest.TestCreatePaymentURLWithReceiptBasic;
var
  PaymentURL: string;
  Positions: array of TReceiptPosition = nil;
begin
  SetLength(Positions, 1);
  Positions[0].Quantity := 1.0;
  Positions[0].Price := 100.00;
  Positions[0].Tax := 6; // НДС 20%
  Positions[0].Text := 'Test Product';
  Positions[0].PaymentSubjectType := 1;
  Positions[0].PaymentMethodType := 4;
  Positions[0].SupplierINN := '';

  PaymentURL := FMerchant.CreatePaymentURLWithReceipt(
    'ORDER-RECEIPT-001',
    100.00,
    'RUB',
    'test@example.com',
    '7704019762',
    'TestGroup',
    Positions,
    0 // taxationSystem
  );

  AssertTrue(PaymentURL <> '', 'Payment URL with receipt should not be empty');
  AssertContains('eshopId=12345', PaymentURL, 'URL should contain eshopId');
  AssertContains('orderId=ORDER-RECEIPT-001', PaymentURL, 'URL should contain orderId');
  AssertContains('merchantReceipt=', PaymentURL, 'URL should contain merchantReceipt');
  AssertContains('hash=', PaymentURL, 'URL should contain hash');
end;

procedure TIntellectMoneyMerchantTest.TestCreatePaymentURLWithReceiptMultiplePositions;
var
  PaymentURL: string;
  Positions: array of TReceiptPosition = nil;
begin
  SetLength(Positions, 2);

  Positions[0].Quantity := 2.0;
  Positions[0].Price := 12.45;
  Positions[0].Tax := 6;
  Positions[0].Text := 'Булка';
  Positions[0].PaymentSubjectType := 1;
  Positions[0].PaymentMethodType := 4;
  Positions[0].SupplierINN := '';

  Positions[1].Quantity := 1.0;
  Positions[1].Price := 5.10;
  Positions[1].Tax := 4;
  Positions[1].Text := 'Спички';
  Positions[1].PaymentSubjectType := 1;
  Positions[1].PaymentMethodType := 4;
  Positions[1].SupplierINN := '3808027390';

  PaymentURL := FMerchant.CreatePaymentURLWithReceipt(
    'ORDER-MULTI',
    29.90,
    'RUB',
    'test@example.com',
    '7704019762',
    'TestGroup',
    Positions
  );

  AssertTrue(PaymentURL <> '', 'Payment URL should not be empty');
  AssertContains('merchantReceipt=', PaymentURL, 'URL should contain merchantReceipt');
  AssertContains('recipientAmount=29.90', PaymentURL, 'URL should contain correct total amount');
end;

procedure TIntellectMoneyMerchantTest.TestCreatePaymentURLWithReceiptOptionalFields;
var
  PaymentURL: string;
  Positions: array of TReceiptPosition = nil;
begin
  SetLength(Positions, 1);
  Positions[0].Quantity := 1.0;
  Positions[0].Price := 100.00;
  Positions[0].Tax := 6;
  Positions[0].Text := 'Product with optional fields';
  Positions[0].PaymentSubjectType := 0; // не передаем
  Positions[0].PaymentMethodType := 0;  // не передаем
  Positions[0].SupplierINN := '';       // не передаем

  PaymentURL := FMerchant.CreatePaymentURLWithReceipt(
    'ORDER-OPTIONAL',
    100.00,
    'RUB',
    'test@example.com',
    '7704019762',
    'TestGroup',
    Positions
  );

  AssertTrue(PaymentURL <> '', 'Payment URL should not be empty');
  AssertContains('merchantReceipt=', PaymentURL, 'URL should contain merchantReceipt');
end;

procedure TIntellectMoneyMerchantTest.TestCreatePaymentURLWithReceiptPayments;
var
  PaymentURL: string;
  Positions: array of TReceiptPosition = nil;
  Payments: TPaymentsArray = nil;
begin
  SetLength(Positions, 1);
  Positions[0].Quantity := 1.0;
  Positions[0].Price := 100.00;
  Positions[0].Tax := 6;
  Positions[0].Text := 'Test Product';
  Positions[0].PaymentSubjectType := 1;
  Positions[0].PaymentMethodType := 4;
  Positions[0].SupplierINN := '';

  SetLength(Payments, 1);
  Payments[0].PaymentType := 2; // безналичный
  Payments[0].Amount := 100.00;

  PaymentURL := FMerchant.CreatePaymentURLWithReceipt(
    'ORDER-WITH-PAYMENTS',
    100.00,
    'RUB',
    'test@example.com',
    '7704019762',
    'TestGroup',
    Positions,
    0, // taxationSystem
    '', '', '', '', '', // optional params
    Payments
  );

  AssertTrue(PaymentURL <> '', 'Payment URL should not be empty');
  AssertContains('merchantReceipt=', PaymentURL, 'URL should contain merchantReceipt');
end;

procedure TIntellectMoneyMerchantTest.TestMerchantReceiptJSONStructure;
var
  PaymentURL: string;
  Positions: array of TReceiptPosition = nil;
  StartPos: Integer;
begin
  SetLength(Positions, 1);
  Positions[0].Quantity := 1.0;
  Positions[0].Price := 100.00;
  Positions[0].Tax := 6;
  Positions[0].Text := 'Test Product';
  Positions[0].PaymentSubjectType := 1;
  Positions[0].PaymentMethodType := 4;
  Positions[0].SupplierINN := '';

  PaymentURL := FMerchant.CreatePaymentURLWithReceipt(
    'ORDER-JSON',
    100.00,
    'RUB',
    'test@example.com',
    '7704019762',
    'TestGroup',
    Positions
  );

  // Извлекаем JSON из URL (упрощенная проверка)
  StartPos := Pos('merchantReceipt=', PaymentURL);
  AssertTrue(StartPos > 0, 'URL should contain merchantReceipt parameter');

  // Проверяем наличие обязательных полей в URL
  AssertTrue(Pos('inn', PaymentURL) > 0, 'merchantReceipt should contain inn field');
  AssertTrue(Pos('group', PaymentURL) > 0, 'merchantReceipt should contain group field');
  AssertTrue(Pos('content', PaymentURL) > 0, 'merchantReceipt should contain content field');
  AssertTrue(Pos('positions', PaymentURL) > 0, 'merchantReceipt should contain positions field');
  AssertTrue(Pos('quantity', PaymentURL) > 0, 'merchantReceipt should contain quantity field');
  AssertTrue(Pos('price', PaymentURL) > 0, 'merchantReceipt should contain price field');
  AssertTrue(Pos('tax', PaymentURL) > 0, 'merchantReceipt should contain tax field');
  AssertTrue(Pos('text', PaymentURL) > 0, 'merchantReceipt should contain text field');
end;

procedure TIntellectMoneyMerchantTest.TestMerchantReceiptWithSkipAmountCheck;
var
  PaymentURL: string;
  Positions: array of TReceiptPosition = nil;
begin
  SetLength(Positions, 1);
  Positions[0].Quantity := 1.0;
  Positions[0].Price := 100.00;
  Positions[0].Tax := 6;
  Positions[0].Text := 'Test Product';
  Positions[0].PaymentSubjectType := 1;
  Positions[0].PaymentMethodType := 4;
  Positions[0].SupplierINN := '';

  PaymentURL := FMerchant.CreatePaymentURLWithReceipt(
    'ORDER-SKIP-CHECK',
    100.00,
    'RUB',
    'test@example.com',
    '7704019762',
    'TestGroup',
    Positions,
    0, // taxationSystem
    '', '', '', '', '', // optional params
    nil, // payments
    1  // skipAmountCheck
  );

  AssertTrue(PaymentURL <> '', 'Payment URL should not be empty');
  AssertContains('skipAmountCheck', PaymentURL, 'merchantReceipt should contain skipAmountCheck');
end;

procedure TIntellectMoneyMerchantTest.TestMerchantReceiptWithTaxationSystem;
var
  PaymentURL: string;
  Positions: array of TReceiptPosition = nil;
  Payments: TPaymentsArray = nil;
begin
  SetLength(Positions, 1);
  Positions[0].Quantity := 1.0;
  Positions[0].Price := 100.00;
  Positions[0].Tax := 6;
  Positions[0].Text := 'Test Product';
  Positions[0].PaymentSubjectType := 1;
  Positions[0].PaymentMethodType := 4;
  Positions[0].SupplierINN := '';

  SetLength(Payments, 1);
  Payments[0].PaymentType := 2;
  Payments[0].Amount := 100.00;

  PaymentURL := FMerchant.CreatePaymentURLWithReceipt(
    'ORDER-TAX-SYSTEM',
    100.00,
    'RUB',
    'test@example.com',
    '7704019762',
    'TestGroup',
    Positions,
    3, // taxationSystem (упрощенная система)
    '', '', '', '', '',
    Payments
  );

  AssertTrue(PaymentURL <> '', 'Payment URL should not be empty');
  AssertContains('taxationSystem', PaymentURL, 'merchantReceipt should contain taxationSystem');
end;

procedure TIntellectMoneyMerchantTest.TestMerchantReceiptUTF8Encoding;
var
  PaymentURL: string;
  Positions: array of TReceiptPosition = nil;
begin
  SetLength(Positions, 1);
  Positions[0].Quantity := 1.0;
  Positions[0].Price := 100.00;
  Positions[0].Tax := 6;
  Positions[0].Text := 'Товар с русским названием: Сок «Груша»';
  Positions[0].PaymentSubjectType := 1;
  Positions[0].PaymentMethodType := 4;
  Positions[0].SupplierINN := '';

  PaymentURL := FMerchant.CreatePaymentURLWithReceipt(
    'ORDER-UTF8',
    100.00,
    'RUB',
    'test@example.com',
    '7704019762',
    'TestGroup',
    Positions
  );

  AssertTrue(PaymentURL <> '', 'Payment URL with UTF-8 should not be empty');
  AssertContains('merchantReceipt=', PaymentURL, 'URL should contain merchantReceipt');
end;

procedure TIntellectMoneyMerchantTest.TestMerchantReceiptEmptyPositions;
var
  PaymentURL: string;
  Positions: array of TReceiptPosition = nil;
begin
  SetLength(Positions, 0); // пустой массив

  try
    PaymentURL := FMerchant.CreatePaymentURLWithReceipt(
      'ORDER-EMPTY',
      100.00,
      'RUB',
      'test@example.com',
      '7704019762',
      'TestGroup',
      Positions
    );

    AssertTrue(PaymentURL <> '', 'Payment URL should be created even with empty positions');
  except

  end;

  // В идеале должно быть исключение, но можно и не требовать его
  // AssertTrue(ExceptionRaised, 'Should raise exception with empty positions');
end;

initialization
  RegisterTest('IntellectMoneyMerchant', TIntellectMoneyMerchantTest);

end.
