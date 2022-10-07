# encoding: UTF-8
# WSDL4R - Creating MappingRegistry support.
# Copyright (C) 2000-2007  NAKAMURA, Hiroshi <nahi@ruby-lang.org>.

# This program is copyrighted free software by NAKAMURA, Hiroshi.  You can
# redistribute it and/or modify it under the same terms of Ruby's license;
# either the dual license version in 2003, or any later version.


require 'wsdl/soap/classDefCreatorSupport'


module WSDL
module SOAP


# requires @defined_const = {}, @dump_with_inner, @modulepath
module WorkatoSchemaCreatorSupport
  include ClassDefCreatorSupport
  include XSD::CodeGen

  NS_VERSION = "2022_2"

  def dump_with_inner
    @dump_with_inner = []
    @dump_with_inner.unshift(yield)
    @dump_with_inner.join("\n")
  end

  def simple_enums
    @simple_enums ||= HashWithIndifferentAccess.new
  end

  def namespace_const
    @namespace_const ||= HashWithIndifferentAccess.new
  end

  def dump_complextypedef(mpath, qname, typedef, as_element = nil, opt = {})
    #puts "COMPTYPE :: #{typedef.compoundtype}" unless %w(TYPE_STRUCT TYPE_EMPTY TYPE_ARRAY TYPE_SIMPLE).include?(typedef.compoundtype.to_s)
    case typedef.compoundtype
    when :TYPE_STRUCT, :TYPE_EMPTY
      dump_complex_typemap(mpath, qname, typedef, as_element, opt)
    when :TYPE_ARRAY
      dump_array_typemap(mpath, qname, typedef, as_element, opt)
    when :TYPE_SIMPLE
      dump_simple_typemap(mpath, qname, typedef, as_element, opt)
    when :TYPE_MAP
      # mapped as a general Hash
      nil
    else
      raise RuntimeError.new(
        "unknown kind of complexContent: #{typedef.compoundtype}")
    end
  end

  def dump_array_typemap(mpath, qname, typedef, as_element, opt)
    #puts "NAME :: #{qname.to_s}"
    if typedef.find_soapenc_arytype
      if opt[:encoded]
        dump_encoded_array_typemap(mpath, qname, typedef, as_element, opt)
      end
    else
      dump_literal_array_typemap(mpath, qname, typedef, as_element, opt)
    end
  end

  def dump_complex_typemap(mpath, qname, typedef, as_element, opt)
    var = {}
    define_dump_class(var, mpath, qname, typedef, as_element, opt)
    schema_ns = (var[:schema_name] || var[:schema_type]).namespace
    if var[:schema_type] and typedef.base
      var[:schema_basetype] = typedef.base
    end
    parentmodule = var[:class]
    parsed_element =
      parse_elements(typedef.elements, qname.namespace, parentmodule, opt)

    if typedef.choice?
      parsed_element.unshift(:choice)
    end
    #puts "SCHEMA :: #{var[:schema]}"
    var[:schema_element] = dump_schema_element_definition(var, parsed_element, opt.merge(indent: 0))
    unless typedef.attributes.empty?
      var[:schema_attribute] = define_attribute(var, typedef.attributes, opt)
    end
    assign_const(schema_ns, 'Ns')
    #puts "VAR :: #{var}" if typedef.abstract
    #puts "VAR1 :: #{var}" if var[:schema].include?("CustomizationRefList")
    [dump_entry(@varname, var, opt), var[:dependents]]
  end

  def dump_simple_typemap(mpath, qname, typedef, as_element, opt)
    var = {}
    define_dump_class(var, mpath, qname, typedef, as_element, opt)
    schema_ns = (var[:schema_name] || var[:schema_type]).namespace
    unless typedef.attributes.empty?
      var[:schema_attribute] = define_attribute(var, typedef.attributes, opt)
    end
    assign_const(schema_ns, 'Ns')
    [dump_entry(@varname, var, opt), var[:dependents]]
  end

  def namespace_mapping
    @namespace_mapping ||= {
      "NsCore_#{NS_VERSION}PlatformWebservicesNetsuiteCom" => "platformCore",
      "NsFaults_#{NS_VERSION}PlatformWebservicesNetsuiteCom" => "platformFaults",
      "NsMessages_#{NS_VERSION}PlatformWebservicesNetsuiteCom" => "platformMessages",
      "NsCommon_#{NS_VERSION}PlatformWebservicesNetsuiteCom" => "platformCommon",
      "NsScheduling_#{NS_VERSION}ActivitiesWebservicesNetsuiteCom" => "activitiesScheduling",
      "NsCommunication_#{NS_VERSION}GeneralWebservicesNetsuiteCom" => "generalCommunication",
      "NsFilecabinet_#{NS_VERSION}DocumentsWebservicesNetsuiteCom" => "documentsFileCabinet",
      "NsRelationships_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsRelationships",
      "NsSupport_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsSupport",
      "NsAccounting_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsAccounting",
      "NsSales_#{NS_VERSION}TransactionsWebservicesNetsuiteCom" => "transactionsSales",
      "NsPurchases_#{NS_VERSION}TransactionsWebservicesNetsuiteCom" => "transactionsPurchases",
      "NsCustomers_#{NS_VERSION}TransactionsWebservicesNetsuiteCom" => "transactionsCustomers",
      "NsFinancial_#{NS_VERSION}TransactionsWebservicesNetsuiteCom" => "transactionsFinancial",
      "NsBank_#{NS_VERSION}TransactionsWebservicesNetsuiteCom" => "transactionsBank",
      "NsInventory_#{NS_VERSION}TransactionsWebservicesNetsuiteCom" => "transactionsInventory",
      "NsGeneral_#{NS_VERSION}TransactionsWebservicesNetsuiteCom" => "transactionsGeneral",
      "NsCustomization_#{NS_VERSION}SetupWebservicesNetsuiteCom" => "setupCustomization",
      "NsEmployees_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsEmployees",
      "NsWebsite_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsWebsite",
      "NsEmployees_#{NS_VERSION}TransactionsWebservicesNetsuiteCom" => "transactionsEmployees",
      "NsMarketing_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsMarketing",
      "NsDemandplanning_#{NS_VERSION}TransactionsWebservicesNetsuiteCom" => "transactionsDemandPlanning",
      "NsSupplychain_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsSupplyChain",
      "NsAccountingTypes_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsAccountingTypes",
      "NsCommunicationTypes_#{NS_VERSION}GeneralWebservicesNetsuiteCom" => "generalCommunicationTypes",
      "NsCustomizationTypes_#{NS_VERSION}SetupWebservicesNetsuiteCom" => "setupCustomizationTypes",
      "NsDemandPlanningTypes_#{NS_VERSION}TransactionsWebservicesNetsuiteCom" => "transactionsDemandplanningTypes",
      "NsEmployeeTypes_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsEmployeesTypes",
      "NsFileCabinetTypes_#{NS_VERSION}DocumentsWebservicesNetsuiteCom" => "documentsFilecabinetTypes",
      "NsInventoryTypes_#{NS_VERSION}TransactionsWebservicesNetsuiteCom" => "transactionsInventoryTypes",
      "NsMarketingTypes_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsMarketingTypes",
      "NsRelationshipTypes_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsRelationshipsTypes",
      "NsSaleTypes_#{NS_VERSION}TransactionsWebservicesNetsuiteCom" => "transactionsSalesTypes",
      "NsSchedulingTypes_#{NS_VERSION}ActivitiesWebservicesNetsuiteCom" => "activitiesSchedulingTypes",
      "NsSupplyChainTypes_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsSupplychainTypes",
      "NsSupportTypes_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsSupportTypes",
      "NsWebsiteTypes_#{NS_VERSION}ListsWebservicesNetsuiteCom" => "listsWebsiteTypes"
    }
  end

  def dump_schema_element_definition(var, definition, **opts)
    return '' if definition.empty?
    sp = ' ' * (opts[:indent] || 0)
    ele_type = opts[:ele_type] || :element
    if definition[0].is_a?(::Array)
      #puts "DEFINITION #{definition}"
      dump_schema_element(var, definition, opts.merge(indent: opts[:indent] + 2))
    else
      varname, name, type, occurrence, ns = definition
      #puts "VAR :: #{caller} :: #{definition}" if %w(class v_class).include?(varname)
      is_array = type.include?("[]")
      type.gsub!(/\[\]$/, "") if is_array
      nsm = namespace_mapping[namespace_contantize(ns)] || ""
      if var[:schema].gsub("::", "") == "RecordRef" && opts[:action] == "output" && %w(external_id type).include?(varname)
        return ""
      end
      case type
      when "SOAP::SOAPString"
        if is_array
          sp + "array '#{varname}', of: 'string', control_type: 'text', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
        else
          sp + "string '#{varname}', control_type: 'text', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
        end
      when "SOAP::SOAPBase64"
        if is_array
          sp + "array '#{varname}', of: 'string', control_type: 'text', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
        else
          sp + "string '#{varname}', label: 'Base64 encoded #{varname}', control_type: 'text', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}', hint: 'Base64 encode file content'"
        end
      when "SOAP::SOAPBoolean"
        if is_array
          sp + "array '#{varname}', of: 'boolean', control_type: 'checkbox', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
        else
          sp + "boolean '#{varname}', control_type: 'checkbox', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
        end
      when "SOAP::SOAPInt", "SOAP::SOAPDouble", "SOAP::SOAPLong"
        if is_array
          sp + "array '#{varname}', of: 'number', control_type: 'text', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
        else
          sp + "number '#{varname}', control_type: 'text', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
        end
      when "SOAP::SOAPDateTime"
        if is_array
          sp + "array '#{varname}', of: 'date_time', control_type: 'date_time', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
        else
          sp + "date_time '#{varname}', control_type: 'date_time', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
        end
      #when "RecordRef"
      #  sp + "string '#{varname}', control_type: 'text', netsuite_type: 'record_ref, ns_tag: '#{ns}'"
      when "SearchBooleanField", "SearchColumnBooleanField"
        sp + "boolean '#{varname}', control_type: 'checkbox', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}', ns_search_type: '#{type}'"
      when "SearchStringField", "SearchTextNumberField", "SearchMultiSelectField", "SearchEnumMultiSelectField",
           "SearchColumnStringField", "SearchColumnTextNumberField", "SearchColumnEnumSelectField", "SearchColumnSelectField"
        sp + "string '#{varname}', control_type: 'text', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}', ns_search_type: '#{type}'"
      when "SearchLongField", "SearchDoubleField", "SearchColumnLongField", "SearchColumnDoubleField"
        sp + "number '#{varname}', control_type: 'text', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}', ns_search_type: '#{type}'"
      when "SearchDateField"
        sp + "object '#{varname}', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}', ns_search_type: '#{type}' do\n" +
        sp + "  date_time 'from', control_type: 'date_time', optional: true\n" +
        sp + "  date_time 'to', control_type: 'date_time', optional: true\n" +
        sp + "end"
      when "SearchColumnDateField"
        sp + "date_time '#{varname}', control_type: 'date_time', optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}', ns_search_type: '#{type}'"
      else
        if is_array
          if (enum = simple_enums[type]).present?
            sp + "array '#{varname}', of: :string, optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
          else
            (var[:dependents] ||= []) << "#{type}_#{opts[:action]}"
            #puts "DEPENDENTS :: #{var}" if varname == "customizationRef"
            sp + "array '#{varname}', ref: :#{type}_#{opts[:action]}, optional: true, ns_ref_type: '#{type}', ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
          end
        elsif (enum = simple_enums[type]).present?
          sp + "string '#{varname}', control_type: :select, pick_list: [#{enum}], optional: true, ns_content_type: '#{ele_type}', ns_tag: '#{nsm}',\n" +
            sp + "  **toggle('#{varname}', :string, control_type: 'text', optional: true, toggle_to_primary_hint: 'Select from list', toggle_to_secondary_hint: 'Enter custom value', ns_content_type: '#{ele_type}', ns_tag: '#{nsm}')"
        elsif type == 'RecordRef' && opts[:action] == "input"
          sp + "string '#{varname}', control_type: 'text', label: '#{varname.titleize} ID', optional: true, ns_ref: '#{type}_#{opts[:action]}', ns_content_type: '#{ele_type}', ns_tag: '#{nsm}',\n" +
            sp + "  **toggle('#{varname}_ext_id', :string, control_type: 'text', label: '#{varname.titleize} External ID', optional: true, toggle_to_primary_hint: 'Enter internal ID', toggle_to_secondary_hint: 'Enter external ID', ns_ref: '#{type}_#{opts[:action]}_ext_id', ns_content_type: '#{ele_type}', ns_tag: '#{nsm}')"
        else
          (var[:dependents] ||= []) << "#{type}_#{opts[:action]}"
          if ["NullField", "CustomFieldList", "SearchCustomFieldList", "SearchColumnCustomFieldList"].include?(type)
            sp + "# object '#{varname}', ref: :#{type}_#{opts[:action]}, optional: true, ns_ref_type: '#{type}', ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
          else
            sp + "object '#{varname}', ref: :#{type}_#{opts[:action]}, optional: true, ns_ref_type: '#{type}', ns_content_type: '#{ele_type}', ns_tag: '#{nsm}'"
          end
        end
      end
    end
  end

  def dump_schema_element(var, schema_element, opts)
    delimiter = "\n"
    schema_element.collect { |definition|
      dump_schema_element_definition(var, definition, opts)
    }.compact.join(delimiter)
  end

  def dump_type(name, type)
    if name
      assign_const(name.namespace, 'Ns')
      '[' + ndq(type) + ', ' + dqname(name) + ']'
    else
      ndq(type)
    end
  end

  def dump_occurrence(occurrence)
    if occurrence and occurrence != [1, 1] # default
      minoccurs, maxoccurs = occurrence
      maxoccurs ||= 'nil'
      "[#{minoccurs}, #{maxoccurs}]"
    end
  end

  def parse_elements(elements, base_namespace, mpath, opt)
    schema_element = []
    any = false
    elements.each do |element|
      case element
      when XMLSchema::Any
        # only 1 <any/> is allowed for now.
        raise RuntimeError.new("duplicated 'any'") if any
        any = true
        varname = 'any' # not used
        eleqname = XSD::AnyTypeName
        type = nil
        occurrence = nil
        schema_element << [varname, eleqname, type, occurrence, nil]
      when XMLSchema::Element
        next if element.ref == SchemaName
        typebase = @modulepath
        if element.anonymous_type?
          child_opt = {
            :qualified => (element.elementform == 'qualified'),
            :is_anonymous => true
          }
          @dump_with_inner << dump_complextypedef(mpath, element.name, element.local_complextype, nil, child_opt).first
          typebase = mpath
        end
        type = create_type_name(typebase, element)
        name = name_element(element).name
        varname = safevarname(name)
        if element.map_as_array?
          if type
            type += '[]'
          else
            type = '[]'
          end
        end
        # nil means @@schema_ns + varname
        eleqname = element.name || element.ref
        if eleqname && varname == name && eleqname.namespace == base_namespace
          eleqname = nil
        end
        occurrence = [element.minoccurs, element.maxoccurs]
        puts "VAR :: #{varname}" if varname == "v_class"
        schema_element << [varname, eleqname, type, occurrence, element.name.namespace]
      when WSDL::XMLSchema::Sequence
        child_schema_element =
          parse_elements(element.elements, base_namespace, mpath, opt)
        schema_element << child_schema_element
      when WSDL::XMLSchema::Choice
        child_schema_element =
          parse_elements(element.elements, base_namespace, mpath, opt)
        if !element.map_as_array?
          # choice + maxOccurs="unbounded" is treated just as 'all' now.
          child_schema_element.unshift(:choice)
        end
        schema_element << child_schema_element
      when WSDL::XMLSchema::Group
        if element.content.nil?
          warn("no group definition found: #{element}")
          next
        end
        child_schema_element =
          parse_elements(element.content.elements, base_namespace, mpath, opt)
        schema_element.concat(child_schema_element)
      else
        raise RuntimeError.new("unknown type: #{element}")
      end
    end
    schema_element
  end

  def define_attribute(var, attributes, opt)
    schema_attribute = []
    attributes.each do |attribute|
      name = name_attribute(attribute)
      if klass = attribute_basetype(attribute)
        type = klass.name
      else
        warn("unresolved attribute type #{attribute.type} for #{name}")
        type = nil
      end
      schema_attribute << [name, type]
    end
    delimiter = "\n"
    schema_attribute.collect { |name, type|
      dump_schema_element_definition(var, [safevarname(name.to_s).underscore, name, ndq(type).gsub(/\"/, ""), "[0, 1]", name.namespace], opt.merge(indent: 2, ele_type: :attribute))
    }.compact.join(delimiter)
=begin
    "{\n    " +
      schema_attribute.collect { |name, type|
        assign_const(name.namespace, 'Ns')
        dqname(name) + ' => ' + ndq(type)
      }.join(",\n    ") +
    "\n  }"
=end
    #dump_schema_element_definition(var, [name, name, ndq(type), "[0, 1]", name.namespace], indent = 0)
    #{}""
  end

  def dump_entry(regname, var, opt)
    [
      dump_entry_item(var, :schema, opt[:action]),
      dump_entry_item(var, :schema_element),
      dump_entry_item(var, :schema_attribute)
    ].compact.join("\n") +
    "\nend\n"
  end

  def dump_entry_item(var, key, action = nil)
    if var.key?(key)
      case key
      when :schema
        "#{key} '#{var[key].gsub('::', '')}_#{action}' do"
      when :schema_element, :schema_attribute
        var[key]
      else
        raise "Unknown dump type: #{dump_type}"
      end
    end
  end

  def dump_simpletypedef(mpath, qname, simpletype, as_element = nil, opt = {})
    if simpletype.restriction
      dump_simpletypedef_restriction(mpath, qname, simpletype, as_element, opt)
    # elsif simpletype.list
    #   dump_simpletypedef_list(mpath, qname, simpletype, as_element, opt)
    # elsif simpletype.union
    #   dump_simpletypedef_union(mpath, qname, simpletype, as_element, opt)
    # else
    #   raise RuntimeError.new("unknown kind of simpletype: #{simpletype}")
    end
  end

  def dump_simpletypedef_restriction(mpath, qname, typedef, as_element, opt)
    restriction = typedef.restriction
    unless restriction.enumeration?
      # not supported.  minlength?
      return nil
    end
    var = {}
    define_dump_class(var, mpath, qname, typedef, as_element, opt)
    schema_ns = (var[:schema_name] || var[:schema_type]).namespace
    assign_const(schema_ns, 'Ns')
    { var[:schema] => dump_enum(typedef) }
  end

  def dump_enum(typedef)
    typedef.restriction.enumeration.collect do |enum|
      "[\"#{enum.titleize}\", \"#{enum}\"]"
    end.compact.join(",\n")
  end

  def dump_simpletypedef_list(mpath, qname, typedef, as_element, opt)
    nil
  end

  def dump_simpletypedef_union(mpath, qname, typedef, as_element, opt)
    nil
  end

  DEFAULT_ITEM_NAME = XSD::QName.new(nil, 'item')

  def dump_literal_array_typemap(mpath, qname, typedef, as_element, opt)
    var = {}
    define_dump_class(var, mpath, qname, typedef, as_element, opt)
    schema_ns = (var[:schema_name] || var[:schema_type]).namespace
    parsed_element =
      parse_elements(typedef.elements, qname.namespace, var[:class], opt)
    if parsed_element.empty?
      parsed_element = [create_array_element_definition(typedef, mpath)]
    end
    var[:schema_element] = dump_schema_element_definition(var, parsed_element, opt.merge(indent: 2))
    assign_const(schema_ns, 'Ns')
    [dump_entry(@varname, var, opt), var[:dependents]]
  end

  def dump_encoded_array_typemap(mpath, qname, typedef, as_element, opt)
    arytype = typedef.find_arytype || XSD::AnyTypeName
    type = XSD::QName.new(arytype.namespace, arytype.name.sub(/\[(?:,)*\]$/, ''))
    return <<__EOD__
#{@varname}.set(
  #{mapped_class_name(qname, mpath)},
  ::SOAP::SOAPArray,
  ::SOAP::Mapping::EncodedRegistry::TypedArrayFactory,
  { :type => #{dqname(type)} }
)
__EOD__
  end

  # used when "soapenc:arrayType" definition
  def create_array_element_definition(typedef, mpath)
    child_type = typedef.child_type
    child_element = typedef.find_aryelement
    if child_type == XSD::AnyTypeName
      type = nil
    elsif child_element
      if klass = element_basetype(child_element)
        type = klass.name
      else
        typename = child_element.type || child_element.name
        type = mapped_class_name(typename, mpath)
      end
    elsif child_type
      type = mapped_class_name(child_type, mpath)
    else
      type = nil
    end
    occurrence = [0, nil]
    if child_element and child_element.name
      if child_element.map_as_array?
        type << '[]' if type
        occurrence = [child_element.minoccurs, child_element.maxoccurs]
      end
      child_element_name = child_element.name
    else
      child_element_name = DEFAULT_ITEM_NAME
    end
    [child_element_name.name, child_element_name, type, occurrence, child_element_name.namespace]
  end

  def define_dump_class(var, mpath, qname, typedef, as_element, opt)
    var[:schema] = mapped_class_name(qname, "")
    if as_element
      var[:schema_name] = as_element
      schema_ns = as_element.namespace
    elsif typedef.name.nil?
      var[:schema_name] = qname
      schema_ns = qname.namespace
    else
      var[:schema_type] = qname
      schema_ns = qname.namespace
    end
    var[:is_anonymous] = opt[:is_anonymous] if opt.key?(:is_anonymous)
    # true, false, or nil
    if opt.key?(:qualified)
      var[:schema_qualified] = opt[:qualified].to_s
    end
  end

  def namespace_contantize(value, prefix = 'Ns')
    return namespace_const[value] if !value.present? || namespace_const.key?(value).present?
    name = value.scan(/[^:\/]+\/?\z/)[0] || 'C'
    tag = prefix + safeconstname(name)
    namespace_const[value] = tag
  end
end


end
end
