unit IntellectMoneyMerchantTests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, intellectmoney_merchant, intellectmoney_base
  ;

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
  // Тест с минимальным набором параметров (остальные пустые)
  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-MIN',
    10.00,
    'RUB',
    'minimal@test.com',
    '',  // userName
    '',  // serviceName
    '',  // successUrl
    ''   // failUrl
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
  // Создаем два URL с одинаковыми параметрами
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

  // Извлекаем хеш из URL
  StartPos := Pos('hash=', PaymentURL1);
  AssertTrue(StartPos > 0, 'First URL should contain hash parameter');

  StartPos := Pos('hash=', PaymentURL2);
  AssertTrue(StartPos > 0, 'Second URL should contain hash parameter');

  // Хеши должны совпадать для одинаковых параметров
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
  // Проверяем корректное кодирование специальных символов
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

  // Email должен быть закодирован
  AssertTrue((Pos('test%2Bspecial%40example.com', PaymentURL) > 0)
    OR (Pos('test+special%40example.com', PaymentURL) > 0),
    'Email should be URL encoded');

  // Амперсанд должен быть закодирован
  AssertTrue(Pos('%26', PaymentURL) > 0, 'Ampersand should be encoded');
end;

procedure TIntellectMoneyMerchantTest.TestAmountFormatting;
var
  PaymentURL: string;
begin
  // Проверяем форматирование суммы с разными значениями

  // Целое число
  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-AMT1',
    100.00,
    'RUB',
    'test@example.com'
  );
  AssertContains('recipientAmount=100.00', PaymentURL,
    'Whole number should be formatted with .00');

  // Число с копейками
  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-AMT2',
    99.99,
    'RUB',
    'test@example.com'
  );
  AssertContains('recipientAmount=99.99', PaymentURL,
    'Decimal number should preserve decimal places');

  // Число с одной цифрой после запятой
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
  // По умолчанию язык должен быть 'ru'
  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-LANG',
    100.00,
    'RUB',
    'test@example.com'
  );
  AssertContains('/ru/', PaymentURL, 'Default language should be ru');

  // Меняем язык на английский
  FMerchant.Lang := 'en';
  PaymentURL := FMerchant.CreatePaymentURL(
    'ORDER-LANG-EN',
    100.00,
    'USD',
    'test@example.com'
  );
  AssertContains('/en/', PaymentURL, 'Language should be en when set');
end;

initialization
  RegisterTest('IntellectMoneyMerchant', TIntellectMoneyMerchantTest);

end.

