program gui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, IntellectMoneyClientTests, IntellectMoneyIntegrationTests,
  IntellectMoneyParameterTests
  ;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

