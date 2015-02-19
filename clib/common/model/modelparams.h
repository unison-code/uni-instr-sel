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

#ifndef SOLVERS_COMMON_MODEL_MODEL_PARAMS__
#define SOLVERS_COMMON_MODEL_MODEL_PARAMS__

#include "constraints.h"
#include "types.h"
#include "../exceptions/exception.h"
#include "../json/json.h"
#include <list>
#include <string>
#include <vector>

namespace Model {

/**
 * Contains the parameters which will be used to create an instance of the CP
 * model.
 */
class ModelParams {
  public:
    /**
     * Creates an empty parameter object.
     */
    ModelParams(void);

    /**
     * Destroys this object.
     */
    ~ModelParams(void);

    /**
     * Gets the number of operation nodes in the function graph.
     *
     * @returns Number of nodes.
     */
    size_t
    getNumOperationNodesInF(void) const;

    /**
     * Gets the number of entity nodes in the function graph.
     *
     * @returns Number of nodes.
     */
    size_t
    getNumEntityNodesInF(void) const;

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
     * Gets the list of dominators per given label node in the function graph.
     *
     * @returns List of label nodes that dominates a particular label node.
     */
    std::vector< std::list<ArrayIndex> >
    getLabelDomsetsInF(void) const;

    /**
     * Gets the list of postdominators per given label node in the function
     * graph.
     *
     * @returns List of label nodes that postdominates a particular label node.
     */
    std::vector< std::list<ArrayIndex> >
    getLabelPostDomsetsInF(void) const;

    /**
     * Gets the list of dominance edges per given label node in the function
     * graph.
     *
     * @returns List of entity nodes to which there is a dominance edge from a
     * particular label node.
     */
    std::vector< std::list<ArrayIndex> >
    getLabelDomEdgesInF(void) const;

    /**
     * Gets the list of postdominance edges per given label node in the function
     * graph.
     *
     * @returns List of entity nodes to which there is a postdominance edge from
     * a particular label node.
     */
    std::vector< std::list<ArrayIndex> >
    getLabelPostDomEdgesInF(void) const;

    /**
     * Gets the entry label in the function graph.
     *
     * @returns Array index for a label node.
     */
    ArrayIndex
    getEntryLabelInF(void) const;

    /**
     * Gets the essential operation nodes in the function, which must be
     * covered.
     *
     * @returns List of array indices for operation nodes.
     */
    std::list<ArrayIndex>
    getAllEssentialOpNodesInF(void) const;

    /**
     * Gets execution frequencies per basic block (as identified by the label
     * nodes) in the function graph.
     *
     * @returns The execution frequencies.
     */
    std::vector<int>
    getExecFreqOfAllBBsInF(void) const;

    /**
     * Gets code sizes per match.
     *
     * @returns The code sizes (in bytes).
     */
    std::vector<int>
    getCodeSizesForAllMatches(void) const;

    /**
     * Gets latencies per matches.
     *
     * @returns The latencies (in bytes).
     */
    std::vector<int>
    getLatenciesForAllMatches(void) const;

    /**
     * Gets the function operation nodes covered per matches.
     *
     * @returns List of node IDs.
     */
    std::vector< std::list<ID> >
    getOperationNodesCoveredByAllMatches(void) const;

    /**
     * Gets the function entity nodes defined per match.
     *
     * @returns List of node IDs.
     */
    std::vector< std::list<ID> >
    getEntityNodesDefinedByAllMatches(void) const;

    /**
     * Gets the function entity nodes used per match.
     *
     * @returns List of node IDs.
     */
    std::vector< std::list<ID> >
    getEntityNodesUsedByAllMatches(void) const;

    /**
     * Gets the function label nodes per match that are entries.
     *
     * @returns List of node IDs.
     */
    std::vector< std::list<ID> >
    getEntryLabelNodeOfAllMatches(void) const;

    /**
     * Gets the function label nodes per match that are not entries.
     *
     * @returns List of node IDs.
     */
    std::vector< std::list<ID> >
    getNonEntryLabelNodesInAllMatches(void) const;

    /**
     * Gets the apply-def-dom-use-constraint settings per match.
     *
     * @returns Boolean.
     */
    std::vector<bool>
    getADDUCSettingForAllMatches(void) const;

    /**
     * Gets the constraints for the function graph.
     *
     * @returns Corresponding constraints.
     */
    std::list<const Constraint*>
    getConstraintsForF(void) const;

    /**
     * Gets the constraints per match.
     *
     * @returns Corresponding constraints.
     */
    std::vector< std::list<const Constraint*> >
    getConstraintsForAllMatches(void) const;

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
    parseJson(const std::string& str, ModelParams& p);

  protected:
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
     * Gets a JSON value as an array index.
     *
     * @param value
     *        JSON value.
     * @returns The converted value.
     * @throws Exception
     *         When the value is not of expected type.
     */
    static ArrayIndex
    toArrayIndex(const Json::Value& value);

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
     * Sets the entry label for the function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setEntryLabelInF(const Json::Value& root, ModelParams& p);

    /**
     * Sets the dominator sets for the label nodes in the function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setLabelDomsetsInF(const Json::Value& root, ModelParams& p);

    /**
     * Sets the postdominator sets for the label nodes in the function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setLabelPostDomsetsInF(const Json::Value& root, ModelParams& p);

    /**
     * Sets the dominance edges for the respective label nodes in the function
     * graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setLabelDomEdgesInF(const Json::Value& root, ModelParams& p);

    /**
     * Sets the postdominance edges for the respective label nodes in the
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
    setLabelPostDomEdgesInF(const Json::Value& root, ModelParams& p);

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
    setConstraintsForF(const Json::Value& root, ModelParams& p);

    /**
     * Sets the execution frequencies of the basic blocks (as identified by the
     * label nodes) in the function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setExecFreqOfBBsInF(const Json::Value& root, ModelParams& p);

    /**
     * Sets the essential operation nodes in the function, which must be
     * covered.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setEssentialOpNodesInF(const Json::Value& root, ModelParams& p);

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
    setCodeSizesForMatches(const Json::Value& root, ModelParams& p);

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
    setLatenciesForMatches(const Json::Value& root, ModelParams& p);

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
    setADDUCSettingsForMatches(const Json::Value& root, ModelParams& p);

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
    setOperationNodesCoveredByMatches(const Json::Value& root, ModelParams& p);

    /**
     * Sets the function entity nodes defined by the respective match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setEntityNodesDefinedByMatches(const Json::Value& root, ModelParams& p);

    /**
     * Sets the function entity nodes used by the respective match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setEntityNodesUsedByMatches(const Json::Value& root, ModelParams& p);

    /**
     * Sets the function label nodes that is the entry label of each respective
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
    setEntryLabelNodeOfMatches(const Json::Value& root, ModelParams& p);

    /**
     * Sets the function label nodes that appear in the respective match but
     * not as entries.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setNonEntryLabelNodesInMatches(const Json::Value& root, ModelParams& p);

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
    setConstraintsForMatches(const Json::Value& root, ModelParams& p);

    /**
     * Sets the "number of ..." values.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setNumValues(const Json::Value& root, ModelParams& p);

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
     * Numbers of operation nodes in the function.
     */
    size_t num_func_operation_nodes_;

    /**
     * Numbers of entity nodes in the function.
     */
    size_t num_func_entity_nodes_;

    /**
     * Numbers of label nodes in the function.
     */
    size_t num_func_label_nodes_;

    /**
     * Numbers of registers in the target machine.
     */
    size_t num_regs_;

    /**
     * Numbers of matches.
     */
    size_t num_matches_;

    /**
     * The dominator sets for each label node in the function graph.
     */
    std::vector< std::list<ArrayIndex> > func_label_domsets_;

    /**
     * The postdominator sets for each label node in the function graph.
     */
    std::vector< std::list<ArrayIndex> > func_label_pdomsets_;

    /**
     * The label which indicates the entry point in the function graph.
     */
    ArrayIndex func_entry_label_;

    /**
     * The entity nodes to which the respective label nodes have a dominance
     * edge.
     */
    std::vector< std::list<ArrayIndex> > func_label_dom_edges_;

    /**
     * The entity nodes to which the respective label nodes have a postdominance
     * edge.
     */
    std::vector< std::list<ArrayIndex> > func_label_pdom_edges_;

    /**
     * The essential operation nodes in the function graph, which must be
     * covered.
     */
    std::list<ArrayIndex> func_essential_op_nodes_;

    /**
     * The execution frequency per basic block (as identified by the label
     * nodes) in the function graph.
     */
    std::vector<int> func_bb_exec_freq_;

    /**
     * The constraints for the function graph. The constraints are destroyed
     * when this object is deleted.
     */
    std::list<const Constraint*> func_constraints_;

    /**
     * The code size (in bytes) of the instruction for each match.
     */
    std::vector<int> match_code_sizes_;

    /**
     * The latency (in cycles) of the instruction for each match.
     */
    std::vector<int> match_latencies_;

    /**
     * The operation nodes in the function graph which are covered by each
     * match.
     */
    std::vector< std::list<ArrayIndex> > match_operations_covered_;

    /**
     * The entity nodes in the function graph which are defined by each match.
     */
    std::vector< std::list<ArrayIndex> > match_entities_defined_;

    /**
     * The entity nodes in the function graph which are used by each match.
     */
    std::vector< std::list<ArrayIndex> > match_entities_used_;

    /**
     * The entry label, if any, for each match.
     */
    std::vector< std::list<ArrayIndex> > match_entry_label_;

    /**
     * The label nodes in the function graph which appear in each match but not
     * as entries.
     */
    std::vector< std::list<ArrayIndex> > match_non_entry_labels_;

    /**
     * The constraints for each match. The constraints are destroyed when this
     * object is deleted.
     */
    std::vector< std::list<const Constraint*> > match_constraints_;

    /**
     * Whether def-dom-use constraint should be applied on a particular
     * match. The ability to turn these off are required by the generic phi
     * patterns.
     */
    std::vector<bool> match_apply_def_dom_use_constraint_;
};

}

#endif
