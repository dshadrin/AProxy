----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Logging_Message;

------------------------------------------------------------------------------------------------------------------------
package Logging is

   procedure SendLogMessage(msg : Logging_Message.LogMessage);

end Logging;
