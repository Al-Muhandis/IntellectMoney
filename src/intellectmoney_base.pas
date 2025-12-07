unit intellectmoney_base;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TIntellectMoneyBaseClient }

  TIntellectMoneyBaseClient = class
   private
     FEshopId: string;
     FLang: String;
     FSecretKey: string;
     FBaseUrl: string;
   protected
     property Url: String read FBaseUrl write FBaseUrl;
     property Lang: String read FLang write FLang;
   public
     constructor Create(const aEshopId, aSecretKey: string); virtual;
     property EshopId: string read FEshopId write FEshopId;
     property SecretKey: string read FSecretKey write FSecretKey;
   end;

implementation

{ TIntellectMoneyBaseClient }

constructor TIntellectMoneyBaseClient.Create(const aEshopId, aSecretKey: string);
begin
  inherited Create;
  FEshopId := AEshopId;
  FSecretKey := ASecretKey;
  FLang:='ru';
end;

end.

