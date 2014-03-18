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

#ifndef SOLVER_GECODE_MODEL_MODEL__
#define SOLVER_GECODE_MODEL_MODEL__

#include "../exceptions/exception.h"
#include "../json/json.h"
#include <list>
#include <map>
#include <string>

namespace Model {

typedef unsigned int Id;
typedef unsigned int ArrayIndex;

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
     * Gets the number of action nodes in the function.
     *
     * @returns Number of nodes.
     */
    size_t
    getNumActionNodes(void) const;

    /**
     * Gets the number of entity nodes in the function.
     *
     * @returns Number of nodes.
     */
    size_t
    getNumEntityNodes(void) const;

    /**
     * Gets the number of label nodes in the function.
     *
     * @returns Number of nodes.
     */
    size_t
    getNumLabelNodes(void) const;

    /**
     * Gets the number of pattern instances.
     *
     * @returns Number of instances.
     */
    size_t
    getNumPatternInstances(void) const;

    /**
     * Gets the list of dominators for a given label node in the function graph.
     *
     * @param id
     *        Node ID.
     * @returns List of dominators.
     * @throws Exception
     *         If there is no node with such an ID.
     */
    std::list<Id>
    getDomsetOfLabel(const Id& id) const;

    /**
     * Gets the cost of selecting a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns The cost.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    int
    getCostOfPattern(const Id& instance) const;

    /**
     * Gets the function action nodes covered by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    std::list<Id>
    getActionNodesCoveredByPattern(const Id& instance) const;

    /**
     * Gets the function entity nodes defined by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    std::list<Id>
    getEntityNodesDefinedByPattern(const Id& instance) const;

    /**
     * Gets the function entity nodes used by a particular pattern instance.
     *
     * @param instance
     *        Pattern instance ID.
     * @returns List of node IDs.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    std::list<Id>
    getEntityNodesUsedByPattern(const Id& instance) const;

    /**
     * Gets the array index of a given function action node.
     *
     * @param id
     *        Node ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         If there is no node with such an ID.
     */
    ArrayIndex
    getIndexOfActionNode(const Id& id) const;

    /**
     * Gets the array index of a given function entity node.
     *
     * @param id
     *        Node ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         If there is no node with such an ID.
     */
    ArrayIndex
    getIndexOfEntityNode(const Id& id) const;

    /**
     * Gets the array index of a given function label node.
     *
     * @param id
     *        Node ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         If there is no node with such an ID.
     */
    ArrayIndex
    getIndexOfLabelNode(const Id& id) const;

    /**
     * Gets a list of all the function action node IDs.
     *
     * @returns List of IDs.
     */
    std::list<Id>
    getAllActionNodeIds(void) const;

    /**
     * Gets a list of all the function entity node IDs.
     *
     * @returns List of IDs.
     */
    std::list<Id>
    getAllEntityNodeIds(void) const;

    /**
     * Gets a list of all the function label node IDs.
     *
     * @returns List of IDs.
     */
    std::list<Id>
    getAllLabelNodeIds(void) const;

    /**
     * Gets the array index of a given pattern instance.
     *
     * @param id
     *        Pattern instance ID.
     * @returns Corresponding array index.
     * @throws Exception
     *         If there is no instance with such an ID.
     */
    ArrayIndex
    getIndexOfPattern(const Id& id) const;

    /**
     * Gets a list of all the pattern instance IDs.
     *
     * @returns List of IDs.
     */
    std::list<Id>
    getAllPatternInstanceIds(void) const;

    /**
     * Parses a JSON string into an internal model parameters object.
     *
     * @param str
     *        String containing the JSON data.
     * @param params
     *        The Params object to write to.
     * @throws Exception
     *         When parsing fails.
     */
    static void
    parseJson(const std::string& str, Params& params);

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
    addMapping(const K& key,
               const V& value,
               std::map<K, V>& mapset
    ) {
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
     *        Object to add the mapping to.
     * @returns The value.
     * @throws Exception
     *         If there exists no such mapping.
     */
    template <typename K, typename V>
    static V
    getMappedValue(const K& key,
                   const std::map<K, V>& mapset
    ) {
        typename std::map<K, V>::const_iterator it;
        it = mapset.find(key);
        if (it == mapset.end()) {
            THROW(Exception, "No mapping found");
        }
        return it->second;
    }

    /**
     * Gets a JSON value of certain name from another JSON value.
     *
     * @param value
     *        JSON value to get the field from.
     * @param name
     *        Name of the field to retreive.
     * @returns The value.
     * @throws Exception
     *         When no such field is found.
     */
    static Json::Value
    getJsonValue(const Json::Value& value, const std::string& name);

    /**
     * Gets a JSON value as an Id.
     *
     * @param value
     *        JSON value.
     * @returns The converted value.
     * @throws Exception
     *         When the value is not of expected type.
     */
    static Id
    toId(const Json::Value& value);

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
     * Computes the node ID-to-array index mappings for the action nodes of the
     * function.
     *
     * @param root
     *        The JSON root value.
     * @param param
     *        Object to add the data to.
     */
    static void
    computeMappingsForFunctionActionNodes(const Json::Value& root,
                                          Params& param);

    /**
     * Same as computeMappingsForFunctionActionNodes(const Json::Value&,
     * Params&) but for the entity nodes.
     *
     * @param root
     *        The JSON root value.
     * @param param
     *        Object to add the data to.
     */
    static void
    computeMappingsForFunctionEntityNodes(const Json::Value& root,
                                          Params& param);

    /**
     * Same as computeMappingsForFunctionActionNodes(const Json::Value&,
     * Params&) but for the label nodes, and also sets the dominator sets.
     *
     * @param root
     *        The JSON root value.
     * @param param
     *        Object to add the data to.
     */
    static void
    computeMappingsAndDomsetsForFunctionLabelNodes(const Json::Value& root,
                                                   Params& param);

    /**
     * Same as computeMappingsForFunctionActionNodes(const Json::Value&,
     * Params&) but for the pattern instance IDs.
     *
     * @param root
     *        The JSON root value.
     * @param param
     *        Object to add the data to.
     */
    static void
    computeMappingsForPatterns(const Json::Value& root, Params& param);

    /**
     * Sets the cost values for the pattern instances.
     *
     * @param root
     *        The JSON root value.
     * @param param
     *        Object to add the data to.
     */
    static void
    setPatternCosts(const Json::Value& root, Params& param);

    /**
     * Sets the function action nodes covered by the respective pattern
     * instances.
     *
     * @param root
     *        The JSON root value.
     * @param param
     *        Object to add the data to.
     */
    static void
    setActionNodesCoveredByPatterns(const Json::Value& root, Params& param);

    /**
     * Sets the function entity nodes defined by the respective pattern
     * instances.
     *
     * @param root
     *        The JSON root value.
     * @param param
     *        Object to add the data to.
     */
    static void
    setEntityNodesDefinedByPatterns(const Json::Value& root, Params& param);

    /**
     * Sets the function entity nodes used by the respective pattern instances.
     *
     * @param root
     *        The JSON root value.
     * @param param
     *        Object to add the data to.
     */
    static void
    setEntityNodesUsedByPatterns(const Json::Value& root, Params& param);

  protected:
    /**
     * Maps the node ID of an action node to an array index.
     */
    std::map<Id, ArrayIndex> func_action_node_mappings_;

    /**
     * Same as #func_action_node_mappings_ but for entity nodes.
     */
    std::map<Id, ArrayIndex> func_entity_node_mappings_;

    /**
     * Same as #func_action_node_mappings_ but for label nodes.
     */
    std::map<Id, ArrayIndex> func_label_node_mappings_;

    /**
     * The dominator sets for each label node in the function.
     */
    std::map< Id, std::list<Id> > func_label_domsets_;

    /**
     * Maps the pattern instance ID to an array index.
     */
    std::map<Id, ArrayIndex> pat_inst_mappings_;

    /**
     * The cost of the instruction for each pattern instance.
     */
    std::map<Id, int> pat_inst_costs_;

    /**
     * The action nodes in the function graphs which are covered for each
     * pattern instance.
     */
    std::map< Id, std::list<Id> > pat_inst_actions_covered_;

    /**
     * The entity nodes in the function graphs which are defined for each
     * pattern instance.
     */
    std::map< Id, std::list<Id> > pat_inst_entities_defined_;

    /**
     * The entity nodes in the function graphs which are used for each
     * pattern instance.
     */
    std::map< Id, std::list<Id> > pat_inst_entities_used_;
};

}

#endif
