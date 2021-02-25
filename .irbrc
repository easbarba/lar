require 'irb/completion'

IRB.conf[:IRB_NAME] = "❯"
IRB.conf[:PROMPT_MODE] = :DEFAULT
IRB.conf[:AUTO_INDENT] = true

# IRB.conf[:IRB_RC] = nil
# IRB.conf[:BACK_TRACE_LIMIT]=16
# IRB.conf[:USE_LOADER] = false
# IRB.conf[:USE_MULTILINE] = nil
# IRB.conf[:USE_SINGLELINE] = nil
# IRB.conf[:USE_COLORIZE] = true
# IRB.conf[:USE_TRACER] = false
# IRB.conf[:IGNORE_SIGINT] = true
# IRB.conf[:IGNORE_EOF] = false
# IRB.conf[:PROMPT] = {...}
# IRB.conf[:SAVE_HISTORY] = nil
# IRB.conf[:INSPECT_MODE]=nil

module IRBExtension
  def start
    configure_irb
    configure_auditing
    super
  end

  def configure_irb
    IRB.conf[:PROMPT] ||= {}
    prompt = "#{irb_env}:%03n:%i"
    IRB[:PROMPT][:CUSTOM] = {
      PROMPT_I: "#{prompt}> ",
      PROMPT_N: "#{prompt}> ",
      PROMPT_S: "#{prompt}%l ",
      RETURN: "=> %s\n"
    }
  end

  def irb_env
    case Rails.env
    when development
      Rainbow("development").green
    when "production"
      Rainbow("production").red
    else
      Rainbow(Rails.env.to_s).yellow
    end
  end

  def configure_auditing
    warn(audit_setup_instructions) and return if audit_login.blank? || audit_user_id.blank?

    warn "User Auditing now setup to assign all changes made to a user with you: #{audit_login}@example.com."
    Thread.current.thread_variable_set(:audit_user_id, audit_user_id)
  end

  def audit_user_id
    @audit_user_id ||= User.select(:id).where(login: "#{audit_login}@example.com").id
  end

  def audit_login
    @audit_login ||= ENV.fetch("LOGGED_IN_USER", nil)
  end

  def audit_setup_instructions
    <<~INSTRUCTIONS
    In order to ensure changes you make are properly associated with your account,
    please run the following command with your User's id in place of <user_id>:
    Thread.current.thread_variable_set :audit_user_id, <user_id>
    INSTRUCTIONS
  end
end

module Rails
  class Console
    prepend IRBExtension
  end
end
