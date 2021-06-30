#!/usr/bin/env ruby

# frozen_string_literal: true

module Cejo
  module Ops
    # Take a shot of the marvelous screen
    class Screenshot
      SCREENSHOT_FORMAT = 'png'
      FOLDER = Pathname.new(Dir.home).join('Pictures')
      CURRENT_TIME = Time.new.strftime '%d-%m-%y-%I-%M'

      attr_reader :utils, :mode

      def initialize(utils, mode)
        @utils = utils
        @mode = mode.to_sym if mode
      end

      def shotters
        [flameshot, scrot, maim]
      end

      def shotters_available
        shotters.find_all do |shotter|
          shotter[:bin] if utils.which? shotter[:bin]
        end
      end

      def shotter
        shotters_available.first
      end

      def scrot
        {
          bin: 'scrot',
          full: '--focused --silent',
          partial: '--select --silent',
          folder: FOLDER.join(screenshot_name)
        }
      end

      def flameshot
        {
          bin: 'flameshot',
          full: 'full -p',
          partial: 'gui -p',
          folder: FOLDER.to_path
        }
      end

      def maim
        {
          bin: 'maim',
          full: '',
          partial: '--select',
          folder: FOLDER.join(screenshot_name)
        }
      end

      def screenshot_name
        "screenshot-#{CURRENT_TIME}.#{SCREENSHOT_FORMAT}"
      end

      def final_command
        "#{shotter[:bin]} #{shotter[mode]} #{shotter[:folder]}"
      end

      def run
        system final_command
      end
    end
  end
end
