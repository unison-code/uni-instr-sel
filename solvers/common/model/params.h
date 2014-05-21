/**
 * Copyright (c) 2014, Gabriel Hjort Blindell <ghb@kth.se>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef SOLVERS_COMMON_MODEL_PARAMS__
#define SOLVERS_COMMON_MODEL_PARAMS__

#include "constraints.h"
#include "types.h"
#include "../exceptions/exception.h"
#include "../json/json.h"
#include <list>
#include <map>
#include <string>

namespace Model {

/**
 * Contains the parameters which will be used to create an instance of the CP
 * model.
 */
class Params {
  public:
    /**
     * Creates an empty parameter object.
     */
    Params(void);

    /**
     * Destroys this object.
     */
    ~Params(void);

    /**
     * Gets the number of action nodes in the function graph.
     *
     * @returns Number of nodes.
     */
    size_t
    getNumActionNodesInF(void) const;

    /**
     * Gets the number of data nodes in the function graph.
     *
     * @returns Number of nodes.
     */
    size_t
    getNumDataNodesInF(void) const;

    /**
     * Gets the number of state nodes in the function graph.
     *
     * @returns Number of nodes.
     */
    size_t
    getNumStateNodesInF(void) const;

    /**
     * Gets the number of label nodes in the function graph.
     *
     * @returns Number of nodes.
     */
    size_t
    getNumLabelNodesInF(void) const;

    /**
     * Gets the number of registers in the target machine.
     *
     * @returns Number of registers.
     */
    size_t
    getNumRegistersInM(void) const;

    /**
     * Gets the number of pattern instances.
     *
     * @returns Number of instances.
     */
    size_t
    getNumPIs(void) const;

    /**
     * Gets the list of dominators for a given label node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns List of dominators.
     * @throws Exception
     *         If there is no node with such an ID.
     */
    std::list<ID>
    getDomsetForLabelNodeInF(const ID& id) const;

    /**
     * Gets the root label in the function graph.
     *
     * @returns Node ID.
     */
    ID
    getRootLabelInF(void) const;

    /**
     * Gets the code size of selecting a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns The code size (in bytes).
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    int
    getCodeSizeForPI(const ID& instance) const;

    /**
     * Gets the latency of selecting a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns The latency (in cycles).
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    int
    getLatencyForPI(const ID& instance) const;

    /**
     * Gets the function action nodes covered by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    std::list<ID>
    getActionNodesCoveredByPI(const ID& instance) const;

    /**
     * Gets the function data nodes defined by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    std::list<ID>
    getDataNodesDefinedByPI(const ID& instance) const;

    /**
     * Gets the function state nodes defined by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    std::list<ID>
    getStateNodesDefinedByPI(const ID& instance) const;

    /**
     * Gets the function data nodes used by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    std::list<ID>
    getDataNodesUsedByPI(const ID& instance) const;

    /**
     * Gets the function state nodes used by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    std::list<ID>
    getStateNodesUsedByPI(const ID& instance) const;

    /**
     * Gets the function label nodes referred to by a particular pattern
     * instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    std::list<ID>
    getLabelNodesReferredByPI(const ID& instance) const;

    /**
     * Checks if use-def-dom constraints should be removed for a particular
     * pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns Boolean.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    bool
    getNoUseDefDomConstraintsSettingForPI(const ID& instance) const;

    /**
     * Gets the array index for a given action node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         If there is no node with such an ID.
     */
    ArrayIndex
    getIndexForActionNodeInF(const ID& id) const;

    /**
     * Gets the array index for a given data node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         If there is no node with such an ID.
     */
    ArrayIndex
    getIndexForDataNodeInF(const ID& id) const;

    /**
     * Gets the array index for a given state node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         If there is no node with such an ID.
     */
    ArrayIndex
    getIndexForStateNodeInF(const ID& id) const;

    /**
     * Gets the array index for a given label node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         If there is no node with such an ID.
     */
    ArrayIndex
    getIndexForLabelNodeInF(const ID& id) const;

    /**
     * Gets the array index for a given register in the target machine.
     *
     * @param id
     *        Register ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         If there is no register with such an ID.
     */
    ArrayIndex
    getIndexForRegisterInM(const ID& id) const;

    /**
     * Gets the array index for a given pattern instance.
     *
     * @param id
     *        Pattern instance ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    ArrayIndex
    getIndexForPI(const ID& id) const;

    /**
     * Gets a list of array indices for a given list of action nodes in the
     * function graph.
     *
     * @param ids
     *        List of node ID.
     * @returns List of corresponding array indices.
     * @throws Exception
     *         If there is a node ID with no mapping.
     */
    std::list<ArrayIndex>
    getIndicesForActionNodesInF(const std::list<ID>& ids) const;

    /**
     * Gets a list of array indices for a given list of data nodes in the
     * function graph.
     *
     * @param ids
     *        List of node ID.
     * @returns List of corresponding array indices.
     * @throws Exception
     *         If there is a node ID with no mapping.
     */
    std::list<ArrayIndex>
    getIndicesForDataNodesInF(const std::list<ID>& ids) const;

    /**
     * Gets a list of array indices for a given list of state nodes in the
     * function graph.
     *
     * @param ids
     *        List of node ID.
     * @returns List of corresponding array indices.
     * @throws Exception
     *         If there is a node ID with no mapping.
     */
    std::list<ArrayIndex>
    getIndicesForStateNodesInF(const std::list<ID>& ids) const;

    /**
     * Gets a list of array indices for a given list of label nodes in the
     * function graph.
     *
     * @param ids
     *        List of node ID.
     * @returns List of corresponding array indices.
     * @throws Exception
     *         If there is a node ID with no mapping.
     */
    std::list<ArrayIndex>
    getIndicesForLabelNodesInF(const std::list<ID>& ids) const;

    /**
     * Gets a list of array indices for a given list of machine registers.
     *
     * @param ids
     *        List of register ID.
     * @returns List of corresponding array indices.
     * @throws Exception
     *         If there is a register ID with no mapping.
     */
    std::list<ArrayIndex>
    getIndicesForRegistersInM(const std::list<ID>& ids) const;

    /**
     * Gets a list of IDs for all action nodes in the function graph.
     *
     * @returns List of IDs.
     */
    std::list<ID>
    getIDsForAllActionNodesInF(void) const;

    /**
     * Gets a list of IDs for all data nodes in the function graph.
     *
     * @returns List of IDs.
     */
    std::list<ID>
    getIDsForAllDataNodesInF(void) const;

    /**
     * Gets a list of IDs for all state nodes in the function graph.
     *
     * @returns List of IDs.
     */
    std::list<ID>
    getIDsForAllStateNodesInF(void) const;

    /**
     * Gets a list of IDs for all label nodes in the function graph.
     *
     * @returns List of IDs.
     */
    std::list<ID>
    getIDsForAllLabelNodesInF(void) const;

    /**
     * Gets a list of IDs for all registers in the target machine.
     *
     * @returns List of IDs.
     */
    std::list<ID>
    getIDsForAllRegisterInM(void) const;

    /**
     * Gets a list of IDs for all pattern instances.
     *
     * @returns List of IDs.
     */
    std::list<ID>
    getIDsForAllPIs(void) const;

    /**
     * Gets the constraints for the function graph.
     *
     * @returns Corresponding constraints.
     */
    std::list<const Constraint*>
    getConstraintsForF(void) const;

    /**
     * Gets the constraints for a given pattern instance.
     *
     * @param id
     *        Pattern instance ID.
     * @returns Corresponding constraints.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    std::list<const Constraint*>
    getConstraintsForPI(const ID& id) const;

    /**
     * Checks if a node ID represents an action node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns \c true if action node.
     */
    bool
    isActionNodeInF(const ID& id) const;

    /**
     * Checks if a node ID represents an data node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns \c true if data node.
     */
    bool
    isDataNodeInF(const ID& id) const;

    /**
     * Checks if a node ID represents an state node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns \c true if state node.
     */
    bool
    isStateNodeInF(const ID& id) const;

    /**
     * Checks if a node ID represents an label node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns \c true if label node.
     */
    bool
    isLabelNodeInF(const ID& id) const;

    /**
     * Parses a JSON string into an internal model parameters object.
     *
     * @param str
     *        String containing the JSON data.
     * @param p
     *        The Params object to write to.
     * @throws Exception
     *         When parsing fails.
     */
    static void
    parseJson(const std::string& str, Params& p);

  protected:
    /**
     * Adds a node ID-to-array index mapping to a mapset.
     *
     * @tparam K
     *         Type of key.
     * @tparam V
     *         Type of value.
     * @param key
     *        The key.
     * @param value
     *        The value.
     * @param mapset
     *        Object to add the mapping to.
     */
    template <typename K, typename V>
    static void
    addMapping(const K& key, const V& value, std::map<K, V>& mapset) {
        std::pair< typename std::map<K, V>::iterator, bool > ret;
        ret = mapset.insert(std::pair<K, V>(key, value));
        if (!ret.second) {
            THROW(Exception, "Such a mapping already exists");
        }
    }

    /**
     * Gets a value from a map.
     *
     * @tparam K
     *         Type of key.
     * @tparam V
     *         Type of value.
     * @param key
     *        The key.
     * @param mapset
     *        Object to get the mapping from.
     * @returns The value.
     * @throws Exception
     *         If there exists no such mapping.
     */
    template <typename K, typename V>
    static V
    getMappedValue(const K& key, const std::map<K, V>& mapset) {
        typename std::map<K, V>::const_iterator it;
        it = mapset.find(key);
        if (it == mapset.end()) {
            THROW(Exception, "No mapping found");
        }
        return it->second;
    }

    /**
     * Gets a list of values from a map.
     *
     * @tparam K
     *         Type of key.
     * @tparam V
     *         Type of value.
     * @param keys
     *        The keys.
     * @param mapset
     *        Object to get the mappings from.
     * @returns The values.
     * @throws Exception
     *         If there exists no such mapping.
     */
    template <typename K, typename V>
    static std::list<V>
    getMappedValues(const std::list<K>& keys, const std::map<K, V>& mapset) {
        std::list<V> values;
        for (auto& key : keys) {
            values.push_back(getMappedValue(key, mapset));
        }
        return values;
    }

    /**
     * Gets a list of keys from a map.
     *
     * @tparam K
     *         Type of key.
     * @tparam V
     *         Type of value.
     * @param mapset
     *        Object to get the keys from.
     * @returns The keys.
     */
    template <typename K, typename V>
    static std::list<K>
    getMappedKeys(const std::map<K, V>& mapset) {
        std::list<K> keys;
        for (auto& kv : mapset) {
            keys.push_back(kv.first);
        }
        return keys;
    }

    /**
     * Checks if a JSON field with certain name exists within another JSON
     * value.
     *
     * @param value
     *        JSON value to get the field from.
     * @param name
     *        Name of the field to retreive.
     * @returns \c true if it exists.
     */
    static bool
    hasJsonValue(const Json::Value& value, const std::string& name);

    /**
     * Gets a JSON field with certain name from another JSON value.
     *
     * @param value
     *        JSON value to get the field from.
     * @param name
     *        Name of the field to retreive.
     * @returns The value.
     * @throws Exception
     *         When no such field is found.
     */
    static const Json::Value&
    getJsonValue(const Json::Value& value, const std::string& name);

    /**
     * Gets a JSON value as an ID.
     *
     * @param value
     *        JSON value.
     * @returns The converted value.
     * @throws Exception
     *         When the value is not of expected type.
     */
    static ID
    toID(const Json::Value& value);

    /**
     * Gets a JSON value as an integer.
     *
     * @param value
     *        JSON value.
     * @returns The converted value.
     * @throws Exception
     *         When the value is not of expected type.
     */
    static int
    toInt(const Json::Value& value);

    /**
     * Gets a JSON value as an Boolean.
     *
     * @param value
     *        JSON value.
     * @returns The converted value.
     * @throws Exception
     *         When the value is not of expected type.
     */
    static bool
    toBool(const Json::Value& value);

    /**
     * Gets a JSON value as a string.
     *
     * @param value
     *        JSON value.
     * @returns The converted value.
     * @throws Exception
     *         When the value is not of expected type.
     */
    static std::string
    toString(const Json::Value& value);

    /**
     * Computes the node ID-to-array index mappings for the action nodes of the
     * function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    computeMappingsForActionNodesInF(const Json::Value& root, Params& p);

    /**
     * Same as computeMappingsForFActionNodes(const Json::Value&,
     * Params&) but for the data nodes.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    computeMappingsForDataNodesInF(const Json::Value& root, Params& p);

    /**
     * Same as computeMappingsForFActionNodes(const Json::Value&,
     * Params&) but for the state nodes.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    computeMappingsForStateNodesInF(const Json::Value& root, Params& p);

    /**
     * Same as computeMappingsForFActionNodes(const Json::Value&,
     * Params&) but for the label nodes, and also sets the dominator sets.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    computeMappingsAndDomsetsForLabelNodesInF(
        const Json::Value& root,
        Params& p
    );

    /**
     * Same as computeMappingsForFActionNodes(const Json::Value&, Params&) but
     * for the register in the target machine.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    computeMappingsForRegistersInM(const Json::Value& root, Params& p);

    /**
     * Same as computeMappingsForFActionNodes(const Json::Value&,
     * Params&) but for the pattern instance IDs.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    computeMappingsForPIs(const Json::Value& root, Params& p);

    /**
     * Sets the root label for the function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setRootLabelInF(const Json::Value& root, Params& p);

    /**
     * Sets the constraints of the function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setConstraintsForF(const Json::Value& root, Params& p);

    /**
     * Sets the code size values of the pattern instances.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setCodeSizesForPIs(const Json::Value& root, Params& p);

    /**
     * Sets the latency values of the pattern instances.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setLatenciesForPIs(const Json::Value& root, Params& p);

    /**
     * Sets the no-use-def-dom-constraints settings for the pattern instances.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setNoUseDefDomConstraintsSettingsForPIs(const Json::Value& root, Params& p);

    /**
     * Sets the function action nodes covered by the respective pattern
     * instances.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setActionNodesCoveredByPIs(const Json::Value& root, Params& p);

    /**
     * Sets the function data nodes defined by the respective pattern
     * instances.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setDataNodesDefinedByPIs(const Json::Value& root, Params& p);

    /**
     * Sets the function state nodes defined by the respective pattern
     * instances.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setStateNodesDefinedByPIs(const Json::Value& root, Params& p);

    /**
     * Sets the function data nodes used by the respective pattern instances.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setDataNodesUsedByPIs(const Json::Value& root, Params& p);

    /**
     * Sets the function state nodes used by the respective pattern instances.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setStateNodesUsedByPIs(const Json::Value& root, Params& p);

    /**
     * Sets the function label nodes referred to by the respective pattern
     * instances.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setLabelNodesReferredByPIs(const Json::Value& root, Params& p);

    /**
     * Sets the pattern constraints.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setConstraintsForPIs(const Json::Value& root, Params& p);

    /**
     * Parses a constraint string.
     *
     * @param str
     *        String to parse.
     * @returns Parsed constraint as a newly allocated object.
     * @throws Exception
     *         When the parsing fails.
     */
    static Constraint*
    parseConstraint(const std::string& str);

    /**
     * Parses a Boolean expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    static BoolExpr*
    parseBoolExpr(std::string& str);

    /**
     * Parses a numerical expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    static NumExpr*
    parseNumExpr(std::string& str);

    /**
     * Parses an integer expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    static IntExpr*
    parseIntExpr(std::string& str);

    /**
     * Parses a node ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    static NodeIDExpr*
    parseNodeIDExpr(std::string& str);

    /**
     * Parses a pattern instance ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    static PatternInstanceIDExpr*
    parsePatternInstanceIDExpr(std::string& str);

    /**
     * Parses an instruction ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    static InstructionIDExpr*
    parseInstructionIDExpr(std::string& str);

    /**
     * Parses a pattern ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    static PatternIDExpr*
    parsePatternIDExpr(std::string& str);

    /**
     * Parses a label ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    static LabelIDExpr*
    parseLabelIDExpr(std::string& str);

    /**
     * Parses a register ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    static RegisterIDExpr*
    parseRegisterIDExpr(std::string& str);

    /**
     * Parses a list of register ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed list of expression.
     */
    static std::list<const RegisterIDExpr*>
    parseListOfRegisterIDExpr(std::string& str);

    /**
     * Parses a set expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    static SetExpr*
    parseSetExpr(std::string& str);

    /**
     * Parses a set element expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    static SetElemExpr*
    parseSetElemExpr(std::string& str);

    /**
     * Parses a node ID.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    static ID
    parseNodeID(std::string& str);

    /**
     * Removes initial whitespace from a string.
     *
     * @param str
     *        String to modify.
     */
    static void
    eatWhitespace(std::string& str);

    /**
     * Removes an initial part that matches another string.
     *
     * @param search
     *        Initial part to match.
     * @param str
     *        String to search in and modify (but only if there is match).
     * @returns \c true if there was a match.
     */
    static bool
    eat(const std::string& search, std::string& str);

    /**
     * Removes an initial integer value from a string.
     *
     * @param str
     *        String to modify.
     * @returns An integer.
     * @throws Exception
     *         When the string does not begin with an integer.
     */
    static int
    eatInt(std::string& str);

    /**
     * Destroys the function constraints contained by this object.
     */
    void
    destroyConstraintsForF(void);

    /**
     * Destroys the pattern instance constraints contained by this object.
     */
    void
    destroyConstraintsForPIs(void);

  protected:
    /**
     * Maps the node ID of an action node to an array index.
     */
    std::map<ID, ArrayIndex> func_action_node_mappings_;

    /**
     * Same as #func_action_node_mappings_ but for data nodes.
     */
    std::map<ID, ArrayIndex> func_data_node_mappings_;

    /**
     * Same as #func_action_node_mappings_ but for state nodes.
     */
    std::map<ID, ArrayIndex> func_state_node_mappings_;

    /**
     * Same as #func_action_node_mappings_ but for label nodes.
     */
    std::map<ID, ArrayIndex> func_label_node_mappings_;

    /**
     * The dominator sets for each label node in the function graph.
     */
    std::map< ID, std::list<ID> > func_label_domsets_;

    /**
     * The root label which indicates the entry point in the function graph.
     */
    ID func_root_label_;

    /**
     * The constraints for the function graph. The constraints are destroyed
     * when this object is deleted.
     */
    std::list<const Constraint*> func_constraints_;

    /**
     * Same as #func_action_node_mappings_ but for machine registers.
     */
    std::map<ID, ArrayIndex> mach_reg_mappings_;

    /**
     * Maps the pattern instance ID to an array index.
     */
    std::map<ID, ArrayIndex> pat_inst_mappings_;

    /**
     * The code size (in bytes) of the instruction for each pattern instance.
     */
    std::map<ID, int> pat_inst_code_sizes_;

    /**
     * The latency (in cycles) of the instruction for each pattern instance.
     */
    std::map<ID, int> pat_inst_latencies_;

    /**
     * The action nodes in the function graph which are covered by each pattern
     * instance.
     */
    std::map< ID, std::list<ID> > pat_inst_actions_covered_;

    /**
     * The data nodes in the function graph which are defined by each pattern
     * instance.
     */
    std::map< ID, std::list<ID> > pat_inst_data_defined_;

    /**
     * The data nodes in the function graph which are used by each pattern
     * instance.
     */
    std::map< ID, std::list<ID> > pat_inst_data_used_;

    /**
     * The state nodes in the function graph which are defined by each pattern
     * instance.
     */
    std::map< ID, std::list<ID> > pat_inst_states_defined_;

    /**
     * The state nodes in the function graph which are used by each pattern
     * instance.
     */
    std::map< ID, std::list<ID> > pat_inst_states_used_;

    /**
     * The label nodes in the function graph which are referred to by each
     * pattern instance.
     */
    std::map< ID, std::list<ID> > pat_inst_labels_referred_;

    /**
     * The constraints for each pattern instance. The constraints are destroyed
     * when this object is deleted.
     */
    std::map< ID, std::list<const Constraint*> > pat_inst_constraints_;

    /**
     * Whether use-def-dom constraints should be applied on a particular pattern
     * instance. The ability to turn these off are required by the generic phi
     * patterns.
     */
    std::map<ID, bool> pat_inst_no_use_def_dom_constraints_;
};

}

#endif
