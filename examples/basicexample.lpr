program BasicExample;

{$mode objfpc}{$H+}

uses
  Classes, IntellectMoney
  ;

var
  Client: TIntellectMoneyClient;
  aResponce: String;
begin
  // Инициализация
  Client := TIntellectMoneyClient.Create(
    '471253',           // EshopId
    '3vWFgU177H4FawYs2Y9t',                 // SecretKey
    '',                 // SignSecretKey
    ''                  // BearerToken. Empty value for tests
  );

  
  try
    // Выставление счета
    aResponce := Client.CreateInvoice(
      'ORDER-001',                          // OrderId
      100.00,                               // Сумма
      'TST',                                // Валюта, TST тестовая валюта, RUB - для рублей
      'admin@sigmaparking.ru',               // Email
      'Иван Петров',                        // Имя покупателя
      'Оплата за товары',                   // Назначение платежа
      'https://example.com/success',        // Success URL
      'https://example.com/fail',           // Fail URL
      'https://example.com/back',           // Back URL
      'https://example.com/result',         // Result URL (для уведомлений)
      '2025-12-31 23:59:59',                // Дата истечения
      0,                                    // HoldMode
      0,                                    // HoldTime
      ''                                    // Preference
    );
    
    WriteLn('Response: ' + UTF8ToString(aResponce));
    ReadLn;
    
  finally
    Client.Free;
  end;
end.
