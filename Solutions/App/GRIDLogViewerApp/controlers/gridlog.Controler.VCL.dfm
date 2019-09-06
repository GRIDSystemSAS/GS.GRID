object dmFront: TdmFront
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 348
  Width = 507
  object TimerBus: TTimer
    Interval = 50
    OnTimer = TimerBusTimer
    Left = 240
    Top = 160
  end
end
