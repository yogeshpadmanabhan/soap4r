# encoding: UTF-8
# WSDL4R - Creating EncodedMappingRegistry code from WSDL.
# Copyright (C) 2000-2007  NAKAMURA, Hiroshi <nahi@ruby-lang.org>.

# This program is copyrighted free software by NAKAMURA, Hiroshi.  You can
# redistribute it and/or modify it under the same terms of Ruby's license;
# either the dual license version in 2003, or any later version.


require 'wsdl/info'
require 'wsdl/soap/workatoSchemaCreatorSupport'


module WSDL
  module SOAP
    class WorkatoSchemaCreator
      include WorkatoSchemaCreatorSupport

      attr_reader :definitions

      def initialize(definitions, name_creator, modulepath, ns_version, defined_const)
        puts "INITIALIZE"
        @definitions = definitions
        @name_creator = name_creator
        @modulepath = modulepath
        ns_version_const(ns_version)
        @simpletypes = definitions.collect_simpletypes
        @simpletypes.uniq!
        @complextypes = definitions.collect_complextypes
        @complextypes.uniq!
        @defined_const = defined_const
      end

      def dump
        result = ''
        dump_simpletype
        prepared_types = prepare_complextype
        #puts "PREPARED :: #{prepared_types}"
        str = dump_complextype(prepared_types)
        unless str.empty?
          result << "\n" unless result.empty?
          result << str
        end
=begin
        str = dump_namespace
        unless str.empty?
          result << "\n" unless result.empty?
          result << "\n  namespaces = {"
          result << str
          result << "\n  }"
        end
=end
        result
      end

    private

      def prepare_complextype
        @complextypes.inject({}) do |types, type|
          ["input", "output"].each do |action|
            #puts "TYPE:: #{type} :: #{type.compoundtype}"
            #unless type.abstract
              #puts "TYPES :: #{types} :: #{type} :: #{action}"
              dump_str, dependents = dump_complextypedef(@modulepath, type.name, type, nil, :encoded => true, :action => action)
              types["#{mapped_class_name(type.name, '')}_#{action}"] = [dump_with_inner {
                dump_str
              }, dependents, false]
            #end
          end
          types
        end
      end

      def dump_complextype(prepared_types)
        @dump_data = []
        prepared_types.each do |k, v|
          dump_code(prepared_types, k)
        end
        @dump_data.compact.join("\n")
      end

      def dump_code(prepared_types, parent_type)
        unless prepared_types[parent_type].present?
          puts "MISSING TYPE :: #{parent_type}"
          return
        end
        #puts "parent_type:: #{parent_type}"
        p_type = prepared_types[parent_type]
        if !p_type[2]
          Array.wrap(p_type[1]).each do |type|
            dump_code(prepared_types, type)
          end
          #puts "code :: #{prepared_types[parent_type][0]}"
          #puts "PUSH :: #{parent_type}"
          @dump_data << p_type[0]
          p_type[2] = true
        end
      end

      def dump_simpletype
        @simpletypes.collect { |type|
          h = dump_simpletypedef(@modulepath, type.name, type, nil, :encoded => true)
          #puts "HASH :: #{h.keys}"
          simple_enums.merge!(h)
        }
        #puts "LENGTH :: #{simple_enums.length}"
      end

      def dump_namespace
        namespace_const.collect do |k, v|
          "\"#{v}\" => \"#{k}\""
        end.join(",\n")
      end
    end
  end
end
