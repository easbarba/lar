# -*- ruby -*-

require "irb/completion"

IRB.conf[:IRB_NAME] = "❯"
IRB.conf[:PROMPT_MODE] = :DEFAULT
IRB.conf[:AUTO_INDENT] = true

# This will allow us to access our previous 3 evaluations in an IRB console
IRB.conf[:EVAL_HISTORY] = 3

def verbose_toggle
  irb_context.echo ? irb_context.echo = false : irb_context.echo = true
end

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
      RETURN: "=> %s\n",
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
