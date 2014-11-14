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

struct DefPlaceEdgeData {
    DefPlaceEdgeData(const Model::ID& entity, const Model::ID& label);

    Model::ID entity;
    Model::ID label;
};

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
     * Gets the number of operation nodes in the function graph.
     *
     * @returns Number of nodes.
     */
    size_t
    getNumOperationNodesInF(void) const;

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
     * Gets the number of matches.
     *
     * @returns Number of matches.
     */
    size_t
    getNumMatches(void) const;

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
     * Gets the definition placement edges in the function graph that involves
     * data nodes.
     *
     * @returns List of definition placement edges.
     */
    std::list<DefPlaceEdgeData>
    getDefPlaceEdgesForDataNodesInF(void) const;

    /**
     * Same as getDefPlaceEdgesForDataInF(void) but for state nodes.
     *
     * @returns List of definition placement edges.
     */
    std::list<DefPlaceEdgeData>
    getDefPlaceEdgesForStateNodesInF(void) const;

    /**
     * Gets the root label in the function graph.
     *
     * @returns Node ID.
     */
    Model::ID
    getRootLabelInF(void) const;

    /**
     * Gets the code size of selecting a particular match.
     *
     * @param match
     *        Match ID.
     * @returns The code size (in bytes).
     * @throws Exception
     *         When there is no match with such an ID.
     */
    int
    getCodeSizeForMatch(const Model::ID& match) const;

    /**
     * Gets the latency of selecting a particular match.
     *
     * @param match
     *        Match ID.
     * @returns The latency (in cycles).
     * @throws Exception
     *         When there is no match with such an ID.
     */
    int
    getLatencyForMatch(const Model::ID& match) const;

    /**
     * Gets the function operation nodes covered by a particular match.
     *
     * @param match
     *        Match ID.
     * @returns List of node IDs.
     * @throws Exception
     *         When there is no match with such an ID.
     */
    std::list<Model::ID>
    getOperationNodesCoveredByMatch(const Model::ID& match) const;

    /**
     * Gets the function data nodes defined by a particular match.
     *
     * @param match
     *        Match ID.
     * @returns List of node IDs.
     * @throws Exception
     *         When there is no match with such an ID.
     */
    std::list<Model::ID>
    getDataNodesDefinedByMatch(const Model::ID& match) const;

    /**
     * Gets the function state nodes defined by a particular match.
     *
     * @param match
     *        Match ID.
     * @returns List of node IDs.
     * @throws Exception
     *         When there is no match with such an ID.
     */
    std::list<Model::ID>
    getStateNodesDefinedByMatch(const Model::ID& match) const;

    /**
     * Gets the function data nodes used by a particular match.
     *
     * @param match
     *        Match ID.
     * @returns List of node IDs.
     * @throws Exception
     *         When there is no match with such an ID.
     */
    std::list<Model::ID>
    getDataNodesUsedByMatch(const Model::ID& match) const;

    /**
     * Gets the function state nodes used by a particular match.
     *
     * @param match
     *        Match ID.
     * @returns List of node IDs.
     * @throws Exception
     *         When there is no match with such an ID.
     */
    std::list<Model::ID>
    getStateNodesUsedByMatch(const Model::ID& match) const;

    /**
     * Checks if a particular match has a root label node.
     *
     * @param match
     *        Match ID.
     * @returns Whether the match has a root label node.
     * @throws Exception
     *         When there is no match with such an ID.
     */
    bool
    hasMatchRootLabel(const Model::ID& match) const;

    /**
     * Gets the label node in the function graph that is the root of a
     * particular match.
     *
     * @param match
     *        Match ID.
     * @returns The node ID of the root label.
     * @throws Exception
     *         When there is no match with such an ID, or if the match does not
     *         have a root label.
     */
    Model::ID
    getRootLabelOfMatch(const Model::ID& match) const;

    /**
     * Gets the label nodes in the function graph that appear in a particular
     * match but not as roots.
     *
     * @param match
     *        Match ID.
     * @returns List of node IDs.
     * @throws Exception
     *         When there is no match with such an ID.
     */
    std::list<Model::ID>
    getNonRootLabelNodesInMatch(const Model::ID& match) const;

    /**
     * Checks if def-dom-use constraint should be applied for a particular
     * match.
     *
     * @param match
     *        Match ID.
     * @returns Boolean.
     * @throws Exception
     *         When there is no match with such an ID.
     */
    bool
    getADDUCSettingForMatch(const Model::ID& match) const;

    /**
     * Gets the array index for a given operation node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         When there is no node with such an ID.
     */
    Model::ArrayIndex
    getIndexForOperationNodeInF(const Model::ID& id) const;

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
     * Gets the ID of a given array index for an operation node in the function
     * graph.
     *
     * @param i
     *        Array index.
     * @returns Corresponding node ID.
     * @throws Exception
     *         When there is no node with such an array index.
     */
    Model::ID
    getIDOfOperationNodeInF(const Model::ArrayIndex& i) const;

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
     * Gets the array index for a given match.
     *
     * @param id
     *        Match ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         When there is no match with such an ID.
     */
    Model::ArrayIndex
    getIndexForMatch(const Model::ID& id) const;

    /**
     * Gets the ID of a given array index for a match.
     *
     * @param i
     *        Array index.
     * @returns Corresponding match ID.
     * @throws Exception
     *         When there is no match with such an array index.
     */
    Model::ID
    getIDOfMatch(const Model::ArrayIndex& i) const;

    /**
     * Gets a list of array indices for a given list of operation nodes in the
     * function graph.
     *
     * @param ids
     *        List of node ID.
     * @returns List of corresponding array indices.
     * @throws Exception
     *         When there is a node ID with no mapping.
     */
    std::list<Model::ArrayIndex>
    getIndicesForOperationNodesInF(const std::list<Model::ID>& ids) const;

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
     * Gets a list of IDs of a given list of array indices for operation nodes
     * in the function graph.
     *
     * @param is
     *        List of array indices.
     * @returns List of corresponding node IDs.
     * @throws Exception
     *         When there is an array index with no mapping.
     */
    std::list<Model::ID>
    getIDsOfOperationNodesInF(const std::list<Model::ArrayIndex>& is) const;

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
     * Gets a list of IDs of a given list of array indices for matches.
     *
     * @param is
     *        List of array indices.
     * @returns List of corresponding match IDs.
     * @throws Exception
     *         When there is an array index with no mapping.
     */
    std::list<Model::ID>
    getIDsOfMatches(const std::list<Model::ArrayIndex>& is) const;

    /**
     * Gets a list of IDs for all operation nodes in the function graph.
     *
     * @returns List of IDs.
     */
    std::list<Model::ID>
    getIDsForAllOperationNodesInF(void) const;

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
     * Gets a list of IDs for all matches.
     *
     * @returns List of IDs.
     */
    std::list<Model::ID>
    getIDsForAllMatches(void) const;

    /**
     * Gets the constraints for the function graph.
     *
     * @returns Corresponding constraints.
     */
    std::list<const Model::Constraint*>
    getConstraintsForF(void) const;

    /**
     * Gets the constraints for a given match.
     *
     * @param id
     *        Match ID.
     * @returns Corresponding constraints.
     * @throws Exception
     *         When there is no match with such an ID.
     */
    std::list<const Model::Constraint*>
    getConstraintsForMatch(const Model::ID& id) const;

    /**
     * Checks if a node ID represents an operation node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns \c true if operation node.
     */
    bool
    isOperationNodeInF(const Model::ID& id) const;

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
     * Gets a JSON value as a DefPlaceEdgeData.
     *
     * @param value
     *        JSON value.
     * @returns The converted value.
     * @throws Exception
     *         When the value is not of expected type.
     */
    static DefPlaceEdgeData
    toDefPlaceEdgeData(const Json::Value& value);

    /**
     * Computes the node ID-to-array index mappings for the operation nodes of
     * the function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    computeMappingsForOperationNodesInF(const Json::Value& root, Params& p);

    /**
     * Same as computeMappingsForFOperationNodes(const Json::Value&,
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
     * Same as computeMappingsForFOperationNodes(const Json::Value&,
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
     * Same as computeMappingsForFOperationNodes(const Json::Value&,
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
     * Same as computeMappingsForFOperationNodes(const Json::Value&, Params&)
     * but for the register in the target machine.
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
     * Same as computeMappingsForFOperationNodes(const Json::Value&,
     * Params&) but for the match IDs.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    computeMappingsForMatches(const Json::Value& root, Params& p);

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
     * Sets the definition placement edges in the function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setDefPlaceEdgesForF(const Json::Value& root, Params& p);

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
     * Sets the code size values of the matches.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setCodeSizesForMatches(const Json::Value& root, Params& p);

    /**
     * Sets the latency values of the matches.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setLatenciesForMatches(const Json::Value& root, Params& p);

    /**
     * Sets the apply-def-dom-use-constraint settings for the matches.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setADDUCSettingsForMatches(const Json::Value& root, Params& p);

    /**
     * Sets the function operation nodes covered by the respective match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setOperationNodesCoveredByMatches(const Json::Value& root, Params& p);

    /**
     * Sets the function data nodes defined by the respective match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setDataNodesDefinedByMatches(const Json::Value& root, Params& p);

    /**
     * Sets the function state nodes defined by the respective match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setStateNodesDefinedByMatches(const Json::Value& root, Params& p);

    /**
     * Sets the function data nodes used by the respective match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setDataNodesUsedByMatches(const Json::Value& root, Params& p);

    /**
     * Sets the function state nodes used by the respective match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setStateNodesUsedByMatches(const Json::Value& root, Params& p);

    /**
     * Sets the label nodes in the function graph that appear in the matches but
     * not as roots.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setNonRootLabelNodesInMatches(const Json::Value& root, Params& p);

    /**
     * Sets the function label nodes that is the root label of each respective
     * match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setRootLabelNodeOfMatches(const Json::Value& root, Params& p);

    /**
     * Sets the match constraints.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setConstraintsForMatches(const Json::Value& root, Params& p);

    /**
     * Destroys the function constraints contained by this object.
     */
    void
    destroyConstraintsForF(void);

    /**
     * Destroys the match constraints contained by this object.
     */
    void
    destroyConstraintsForMatches(void);

  protected:
    /**
     * Maps the ID of an operation node in the function graph to an array index.
     */
    std::map<Model::ID, Model::ArrayIndex> func_operation_node_kv_mappings_;

    /**
     * Maps an array index to the ID of an operation node in the function graph.
     */
    std::map<Model::ArrayIndex, Model::ID> func_operation_node_vk_mappings_;

    /**
     * Same as #func_operation_node_kv_mappings_ but for data nodes.
     */
    std::map<Model::ID, Model::ArrayIndex> func_data_node_kv_mappings_;

    /**
     * Same as #func_operation_node_vk_mappings_ but for data nodes.
     */
    std::map<Model::ArrayIndex, Model::ID> func_data_node_vk_mappings_;

    /**
     * Same as #func_operation_node_kv_mappings_ but for state nodes.
     */
    std::map<Model::ID, Model::ArrayIndex> func_state_node_kv_mappings_;

    /**
     * Same as #func_operation_node_vk_mappings_ but for state nodes.
     */
    std::map<Model::ArrayIndex, Model::ID> func_state_node_vk_mappings_;

    /**
     * Same as #func_operation_node_kv_mappings_ but for label nodes.
     */
    std::map<Model::ID, Model::ArrayIndex> func_label_node_kv_mappings_;

    /**
     * Same as #func_operation_node_vk_mappings_ but for label nodes.
     */
    std::map<Model::ArrayIndex, Model::ID> func_label_node_vk_mappings_;

    /**
     * The dominator sets for each label node in the function graph.
     */
    std::map< Model::ID, std::list<Model::ID> > func_label_domsets_;

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
     * The definition placement edges appearing in the function graph.
     */
    std::list<DefPlaceEdgeData> func_def_place_edges_;

    /**
     * Maps the ID of a register in the target machine to an array index.
     */
    std::map<Model::ID, Model::ArrayIndex> mach_reg_kv_mappings_;

    /**
     * Maps an array index to the ID of a register in the target machine.
     */
    std::map<Model::ArrayIndex, Model::ID> mach_reg_vk_mappings_;

    /**
     * Maps the ID of a match to an array index.
     */
    std::map<Model::ID, Model::ArrayIndex> match_kv_mappings_;

    /**
     * Maps an array index to the ID of a match.
     */
    std::map<Model::ArrayIndex, Model::ID> match_vk_mappings_;

    /**
     * The code size (in bytes) of the instruction for each match.
     */
    std::map<Model::ID, int> match_code_sizes_;

    /**
     * The latency (in cycles) of the instruction for each match.
     */
    std::map<Model::ID, int> match_latencies_;

    /**
     * The operation nodes in the function graph which are covered by each
     * match.
     */
    std::map< Model::ID, std::list<Model::ID> > match_operations_covered_;

    /**
     * The data nodes in the function graph which are defined by each match.
     */
    std::map< Model::ID, std::list<Model::ID> > match_data_defined_;

    /**
     * The data nodes in the function graph which are used by each match.
     */
    std::map< Model::ID, std::list<Model::ID> > match_data_used_;

    /**
     * The state nodes in the function graph which are defined by each match.
     */
    std::map< Model::ID, std::list<Model::ID> > match_states_defined_;

    /**
     * The state nodes in the function graph which are used by each match.
     */
    std::map< Model::ID, std::list<Model::ID> > match_states_used_;

    /**
     * The label nodes in the function graph which appear in each match but not
     * as roots.
     */
    std::map< Model::ID, std::list<Model::ID> > match_non_root_labels_;

    /**
     * The root label, if any, for each match.
     */
    std::map<Model::ID, Model::ID> match_root_label_;

    /**
     * The constraints for each match. The constraints are destroyed when this
     * object is deleted.
     */
    std::map< Model::ID, std::list<const Model::Constraint*> >
      match_constraints_;

    /**
     * Whether the def-dom-use constraint should be applied on a particular
     * match. The ability to turn these off are required by the generic phi
     * patterns.
     */
    std::map<Model::ID, bool> match_apply_def_dom_use_constraint_;
};

#endif
