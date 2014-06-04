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

#ifndef SOLVERS_PARAM_PREPROCESSOR_PARAMS__
#define SOLVERS_PARAM_PREPROCESSOR_PARAMS__

#include "common/exceptions/exception.h"
#include "common/json/json.h"
#include "common/model/constraints.h"
#include "common/model/types.h"
#include <list>
#include <map>
#include <string>

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
     *         When there is no node with such an ID.
     */
    std::list<Model::ID>
    getDomsetForLabelNodeInF(const Model::ID& id) const;

    /**
     * Gets the root label in the function graph.
     *
     * @returns Node ID.
     */
    Model::ID
    getRootLabelInF(void) const;

    /**
     * Gets the code size of selecting a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns The code size (in bytes).
     * @throws Exception
     *         When there is no instance with such an ID.
     */
    int
    getCodeSizeForPI(const Model::ID& instance) const;

    /**
     * Gets the latency of selecting a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns The latency (in cycles).
     * @throws Exception
     *         When there is no instance with such an ID.
     */
    int
    getLatencyForPI(const Model::ID& instance) const;

    /**
     * Gets the function action nodes covered by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         When there is no instance with such an ID.
     */
    std::list<Model::ID>
    getActionNodesCoveredByPI(const Model::ID& instance) const;

    /**
     * Gets the function data nodes defined by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         When there is no instance with such an ID.
     */
    std::list<Model::ID>
    getDataNodesDefinedByPI(const Model::ID& instance) const;

    /**
     * Gets the function state nodes defined by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         When there is no instance with such an ID.
     */
    std::list<Model::ID>
    getStateNodesDefinedByPI(const Model::ID& instance) const;

    /**
     * Gets the function data nodes used by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         When there is no instance with such an ID.
     */
    std::list<Model::ID>
    getDataNodesUsedByPI(const Model::ID& instance) const;

    /**
     * Gets the function state nodes used by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         When there is no instance with such an ID.
     */
    std::list<Model::ID>
    getStateNodesUsedByPI(const Model::ID& instance) const;

    /**
     * Gets the function label nodes referred to by a particular pattern
     * instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         When there is no instance with such an ID.
     */
    std::list<Model::ID>
    getLabelNodesReferredByPI(const Model::ID& instance) const;

    /**
     * Checks if use-def-dom constraints should be removed for a particular
     * pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns Boolean.
     * @throws Exception
     *         When there is no instance with such an ID.
     */
    bool
    getNoUseDefDomConstraintsSettingForPI(const Model::ID& instance) const;

    /**
     * Gets the array index for a given action node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         When there is no node with such an ID.
     */
    Model::ArrayIndex
    getIndexForActionNodeInF(const Model::ID& id) const;

    /**
     * Gets the array index for a given data node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         When there is no node with such an ID.
     */
    Model::ArrayIndex
    getIndexForDataNodeInF(const Model::ID& id) const;

    /**
     * Gets the array index for a given state node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         When there is no node with such an ID.
     */
    Model::ArrayIndex
    getIndexForStateNodeInF(const Model::ID& id) const;

    /**
     * Gets the array index for a given label node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         When there is no node with such an ID.
     */
    Model::ArrayIndex
    getIndexForLabelNodeInF(const Model::ID& id) const;

    /**
     * Gets the ID of a given array index for an action node in the function
     * graph.
     *
     * @param i
     *        Array index.
     * @returns Corresponding node ID.
     * @throws Exception
     *         When there is no node with such an array index.
     */
    Model::ID
    getIDOfActionNodeInF(const Model::ArrayIndex& i) const;

    /**
     * Gets the ID for a given array index of an data node in the function
     * graph.
     *
     * @param i
     *        Array index.
     * @returns Corresponding node ID.
     * @throws Exception
     *         When there is no node with such an array index.
     */
    Model::ID
    getIDOfDataNodeInF(const Model::ArrayIndex& i) const;

    /**
     * Gets the ID for a given array index of an state node in the function
     * graph.
     *
     * @param i
     *        Array index.
     * @returns Corresponding node ID.
     * @throws Exception
     *         When there is no node with such an array index.
     */
    Model::ID
    getIDOfStateNodeInF(const Model::ArrayIndex& i) const;

    /**
     * Gets the ID for a given array index of an label node in the function
     * graph.
     *
     * @param i
     *        Array index.
     * @returns Corresponding node ID.
     * @throws Exception
     *         When there is no node with such an array index.
     */
    Model::ID
    getIDOfLabelNodeInF(const Model::ArrayIndex& i) const;

    /**
     * Gets the array index for a given register in the target machine.
     *
     * @param id
     *        Register ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         When there is no register with such an ID.
     */
    Model::ArrayIndex
    getIndexForRegisterInM(const Model::ID& id) const;

    /**
     * Gets the ID of a given array index for a register in the target machine.
     *
     * @param i
     *        Array index.
     * @returns Corresponding register ID.
     * @throws Exception
     *         When there is no register with such an array index.
     */
    Model::ID
    getIDOfRegisterInM(const Model::ArrayIndex& i) const;

    /**
     * Gets the array index for a given pattern instance.
     *
     * @param id
     *        Pattern instance ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         When there is no instance with such an ID.
     */
    Model::ArrayIndex
    getIndexForPI(const Model::ID& id) const;

    /**
     * Gets the ID of a given array index for a pattern instance.
     *
     * @param i
     *        Array index.
     * @returns Corresponding pattern instance ID.
     * @throws Exception
     *         When there is no instance with such an array index.
     */
    Model::ID
    getIDOfPI(const Model::ArrayIndex& i) const;

    /**
     * Gets a list of array indices for a given list of action nodes in the
     * function graph.
     *
     * @param ids
     *        List of node ID.
     * @returns List of corresponding array indices.
     * @throws Exception
     *         When there is a node ID with no mapping.
     */
    std::list<Model::ArrayIndex>
    getIndicesForActionNodesInF(const std::list<Model::ID>& ids) const;

    /**
     * Gets a list of array indices for a given list of data nodes in the
     * function graph.
     *
     * @param ids
     *        List of node ID.
     * @returns List of corresponding array indices.
     * @throws Exception
     *         When there is a node ID with no mapping.
     */
    std::list<Model::ArrayIndex>
    getIndicesForDataNodesInF(const std::list<Model::ID>& ids) const;

    /**
     * Gets a list of array indices for a given list of state nodes in the
     * function graph.
     *
     * @param ids
     *        List of node ID.
     * @returns List of corresponding array indices.
     * @throws Exception
     *         When there is a node ID with no mapping.
     */
    std::list<Model::ArrayIndex>
    getIndicesForStateNodesInF(const std::list<Model::ID>& ids) const;

    /**
     * Gets a list of array indices for a given list of label nodes in the
     * function graph.
     *
     * @param ids
     *        List of node ID.
     * @returns List of corresponding array indices.
     * @throws Exception
     *         When there is a node ID with no mapping.
     */
    std::list<Model::ArrayIndex>
    getIndicesForLabelNodesInF(const std::list<Model::ID>& ids) const;

    /**
     * Gets a list of array indices for a given list of machine registers.
     *
     * @param ids
     *        List of register ID.
     * @returns List of corresponding array indices.
     * @throws Exception
     *         When there is a register ID with no mapping.
     */
    std::list<Model::ArrayIndex>
    getIndicesForRegistersInM(const std::list<Model::ID>& ids) const;

    /**
     * Gets a list of IDs of a given list of array indices for action nodes in
     * the function graph.
     *
     * @param is
     *        List of array indices.
     * @returns List of corresponding node IDs.
     * @throws Exception
     *         When there is an array index with no mapping.
     */
    std::list<Model::ID>
    getIDsOfActionNodesInF(const std::list<Model::ArrayIndex>& is) const;

    /**
     * Gets a list of IDs of a given list of array indices for data nodes in
     * the function graph.
     *
     * @param is
     *        List of array indices.
     * @returns List of corresponding node IDs.
     * @throws Exception
     *         When there is an array index with no mapping.
     */
    std::list<Model::ID>
    getIDsOfDataNodesInF(const std::list<Model::ArrayIndex>& is) const;

    /**
     * Gets a list of IDs of a given list of array indices for state nodes in
     * the function graph.
     *
     * @param is
     *        List of array indices.
     * @returns List of corresponding node IDs.
     * @throws Exception
     *         When there is an array index with no mapping.
     */
    std::list<Model::ID>
    getIDsOfStateNodesInF(const std::list<Model::ArrayIndex>& is) const;

    /**
     * Gets a list of IDs of a given list of array indices for label nodes in
     * the function graph.
     *
     * @param is
     *        List of array indices.
     * @returns List of corresponding node IDs.
     * @throws Exception
     *         When there is an array index with no mapping.
     */
    std::list<Model::ID>
    getIDsOfLabelNodesInF(const std::list<Model::ArrayIndex>& is) const;

    /**
     * Gets a list of IDs of a given list of array indices for registers in the
     * target machine.
     *
     * @param is
     *        List of array indices.
     * @returns List of corresponding register IDs.
     * @throws Exception
     *         When there is an array index with no mapping.
     */
    std::list<Model::ID>
    getIDsOfRegistersInM(const std::list<Model::ArrayIndex>& is) const;

    /**
     * Gets a list of IDs of a given list of array indices for pattern
     * instances.
     *
     * @param is
     *        List of array indices.
     * @returns List of corresponding pattern instance IDs.
     * @throws Exception
     *         When there is an array index with no mapping.
     */
    std::list<Model::ID>
    getIDsOfPIs(const std::list<Model::ArrayIndex>& is) const;

    /**
     * Gets a list of IDs for all action nodes in the function graph.
     *
     * @returns List of IDs.
     */
    std::list<Model::ID>
    getIDsForAllActionNodesInF(void) const;

    /**
     * Gets a list of IDs for all data nodes in the function graph.
     *
     * @returns List of IDs.
     */
    std::list<Model::ID>
    getIDsForAllDataNodesInF(void) const;

    /**
     * Gets a list of IDs for all state nodes in the function graph.
     *
     * @returns List of IDs.
     */
    std::list<Model::ID>
    getIDsForAllStateNodesInF(void) const;

    /**
     * Gets a list of IDs for all label nodes in the function graph.
     *
     * @returns List of IDs.
     */
    std::list<Model::ID>
    getIDsForAllLabelNodesInF(void) const;

    /**
     * Gets a list of IDs for all registers in the target machine.
     *
     * @returns List of IDs.
     */
    std::list<Model::ID>
    getIDsForAllRegisterInM(void) const;

    /**
     * Gets a list of IDs for all pattern instances.
     *
     * @returns List of IDs.
     */
    std::list<Model::ID>
    getIDsForAllPIs(void) const;

    /**
     * Gets the constraints for the function graph.
     *
     * @returns Corresponding constraints.
     */
    std::list<const Model::Constraint*>
    getConstraintsForF(void) const;

    /**
     * Gets the constraints for a given pattern instance.
     *
     * @param id
     *        Pattern instance ID.
     * @returns Corresponding constraints.
     * @throws Exception
     *         When there is no instance with such an ID.
     */
    std::list<const Model::Constraint*>
    getConstraintsForPI(const Model::ID& id) const;

    /**
     * Checks if a node ID represents an action node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns \c true if action node.
     */
    bool
    isActionNodeInF(const Model::ID& id) const;

    /**
     * Checks if a node ID represents an data node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns \c true if data node.
     */
    bool
    isDataNodeInF(const Model::ID& id) const;

    /**
     * Checks if a node ID represents an state node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns \c true if state node.
     */
    bool
    isStateNodeInF(const Model::ID& id) const;

    /**
     * Checks if a node ID represents an label node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns \c true if label node.
     */
    bool
    isLabelNodeInF(const Model::ID& id) const;

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
     *         When there exists no such mapping.
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
     *         When there exists no such mapping.
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
     * Gets a list of all keys present in a mapset.
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
    getAllKeys(const std::map<K, V>& mapset) {
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
    static Model::ID
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
     * Maps the ID of an action node in the function graph to an array index.
     */
    std::map<Model::ID, Model::ArrayIndex> func_action_node_kv_mappings_;

    /**
     * Maps an array index to the ID of an action node in the function graph.
     */
    std::map<Model::ArrayIndex, Model::ID> func_action_node_vk_mappings_;

    /**
     * Same as #func_action_node_kv_mappings_ but for data nodes.
     */
    std::map<Model::ID, Model::ArrayIndex> func_data_node_kv_mappings_;

    /**
     * Same as #func_action_node_vk_mappings_ but for data nodes.
     */
    std::map<Model::ArrayIndex, Model::ID> func_data_node_vk_mappings_;

    /**
     * Same as #func_action_node_kv_mappings_ but for state nodes.
     */
    std::map<Model::ID, Model::ArrayIndex> func_state_node_kv_mappings_;

    /**
     * Same as #func_action_node_vk_mappings_ but for state nodes.
     */
    std::map<Model::ArrayIndex, Model::ID> func_state_node_vk_mappings_;

    /**
     * Same as #func_action_node_kv_mappings_ but for label nodes.
     */
    std::map<Model::ID, Model::ArrayIndex> func_label_node_kv_mappings_;

    /**
     * Same as #func_action_node_vk_mappings_ but for label nodes.
     */
    std::map<Model::ArrayIndex, Model::ID> func_label_node_vk_mappings_;

    /**
     * The dominator sets for each label node in the function graph.
     */
    std::map<Model::ID, std::list<Model::ID> > func_label_domsets_;

    /**
     * The root label which indicates the entry point in the function graph.
     */
    Model::ID func_root_label_;

    /**
     * The constraints for the function graph. The constraints are destroyed
     * when this object is deleted.
     */
    std::list<const Model::Constraint*> func_constraints_;

    /**
     * Maps the ID of a register in the target machine to an array index.
     */
    std::map<Model::ID, Model::ArrayIndex> mach_reg_kv_mappings_;

    /**
     * Maps an array index to the ID of a register in the target machine.
     */
    std::map<Model::ArrayIndex, Model::ID> mach_reg_vk_mappings_;

    /**
     * Maps the ID of a pattern instance to an array index.
     */
    std::map<Model::ID, Model::ArrayIndex> pat_inst_kv_mappings_;

    /**
     * Maps an array index to the ID of a pattern instance.
     */
    std::map<Model::ArrayIndex, Model::ID> pat_inst_vk_mappings_;

    /**
     * The code size (in bytes) of the instruction for each pattern instance.
     */
    std::map<Model::ID, int> pat_inst_code_sizes_;

    /**
     * The latency (in cycles) of the instruction for each pattern instance.
     */
    std::map<Model::ID, int> pat_inst_latencies_;

    /**
     * The action nodes in the function graph which are covered by each pattern
     * instance.
     */
    std::map<Model::ID, std::list<Model::ID> > pat_inst_actions_covered_;

    /**
     * The data nodes in the function graph which are defined by each pattern
     * instance.
     */
    std::map<Model::ID, std::list<Model::ID> > pat_inst_data_defined_;

    /**
     * The data nodes in the function graph which are used by each pattern
     * instance.
     */
    std::map<Model::ID, std::list<Model::ID> > pat_inst_data_used_;

    /**
     * The state nodes in the function graph which are defined by each pattern
     * instance.
     */
    std::map<Model::ID, std::list<Model::ID> > pat_inst_states_defined_;

    /**
     * The state nodes in the function graph which are used by each pattern
     * instance.
     */
    std::map<Model::ID, std::list<Model::ID> > pat_inst_states_used_;

    /**
     * The label nodes in the function graph which are referred to by each
     * pattern instance.
     */
    std::map<Model::ID, std::list<Model::ID> > pat_inst_labels_referred_;

    /**
     * The constraints for each pattern instance. The constraints are destroyed
     * when this object is deleted.
     */
    std::map<Model::ID, std::list<const Model::Constraint*> >
    pat_inst_constraints_;

    /**
     * Whether use-def-dom constraints should be applied on a particular pattern
     * instance. The ability to turn these off are required by the generic phi
     * patterns.
     */
    std::map<Model::ID, bool> pat_inst_no_use_def_dom_constraints_;
};

#endif
