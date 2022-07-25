# -*- ruby -*-

require 'irb/completion'
require 'irb/ext/save-history'

IRB.conf[:IRB_NAME] = '❯'
IRB.conf[:PROMPT_MODE] = :DEFAULT
IRB.conf[:AUTO_INDENT] = true
IRB.conf[:USE_READLINE] = true
IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"
IRB.conf[:EVAL_HISTORY] = 3 # This will allow us to access our previous 3 evaluations in an IRB console
unless IRB.conf[:LOAD_MODULES].include?('irb/completion')
  IRB.conf[:LOAD_MODULES] << 'irb/completion'
end
