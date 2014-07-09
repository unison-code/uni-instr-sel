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
     * Gets the list of dominators per given label node in the function graph.
     *
     * @returns List of dominators.
     */
    std::vector< std::list<ArrayIndex> >
    getDomsetForAllLabelNodesInF(void) const;

    /**
     * Gets the root label in the function graph.
     *
     * @returns Array index for a label node.
     */
    ArrayIndex
    getRootLabelInF(void) const;

    /**
     * Gets code sizes per pattern instance.
     *
     * @returns The code sizes (in bytes).
     */
    std::vector<int>
    getCodeSizesForAllPIs(void) const;

    /**
     * Gets latencies per pattern instances.
     *
     * @returns The latencies (in bytes).
     */
    std::vector<int>
    getLatenciesForAllPIs(void) const;

    /**
     * Gets the function action nodes covered per pattern instances.
     *
     * @returns List of node IDs.
     */
    std::vector< std::list<ID> >
    getActionNodesCoveredByAllPIs(void) const;

    /**
     * Gets the function data nodes defined per pattern instance.
     *
     * @returns List of node IDs.
     */
    std::vector< std::list<ID> >
    getDataNodesDefinedByAllPIs(void) const;

    /**
     * Gets the function state nodes defined per pattern instance.
     *
     * @returns List of node IDs.
     */
    std::vector< std::list<ID> >
    getStateNodesDefinedByAllPIs(void) const;

    /**
     * Gets the function data nodes used per pattern instance.
     *
     * @returns List of node IDs.
     */
    std::vector< std::list<ID> >
    getDataNodesUsedByAllPIs(void) const;

    /**
     * Gets the function state nodes used per pattern instance.
     *
     * @returns List of node IDs.
     */
    std::vector< std::list<ID> >
    getStateNodesUsedByAllPIs(void) const;

    /**
     * Gets the function label nodes referred per pattern instance.
     *
     * @returns List of node IDs.
     */
    std::vector< std::list<ID> >
    getLabelNodesReferredByAllPIs(void) const;

    /**
     * Gets the use-def-dom constraint settings per pattern instance.
     *
     * @returns Boolean.
     */
    std::vector<bool>
    getAUDDCSettingForAllPIs(void) const;

    /**
     * Gets the constraints for the function graph.
     *
     * @returns Corresponding constraints.
     */
    std::list<const Constraint*>
    getConstraintsForF(void) const;

    /**
     * Gets the constraints per pattern instance.
     *
     * @returns Corresponding constraints.
     */
    std::vector< std::list<const Constraint*> >
    getConstraintsForAllPIs(void) const;

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
    setRootLabelInF(const Json::Value& root, ModelParams& p);

    /**
     * Sets the domsets for the label nodes in the function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setDomsetsForLabelNodesInF(const Json::Value& root, ModelParams& p);

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
    setCodeSizesForPIs(const Json::Value& root, ModelParams& p);

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
    setLatenciesForPIs(const Json::Value& root, ModelParams& p);

    /**
     * Sets the apply-use-def-dom-constraints settings for the pattern
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
    setAUDDCSettingsForPIs(const Json::Value& root, ModelParams& p);

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
    setActionNodesCoveredByPIs(const Json::Value& root, ModelParams& p);

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
    setDataNodesDefinedByPIs(const Json::Value& root, ModelParams& p);

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
    setStateNodesDefinedByPIs(const Json::Value& root, ModelParams& p);

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
    setDataNodesUsedByPIs(const Json::Value& root, ModelParams& p);

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
    setStateNodesUsedByPIs(const Json::Value& root, ModelParams& p);

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
    setLabelNodesReferredByPIs(const Json::Value& root, ModelParams& p);

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
    setConstraintsForPIs(const Json::Value& root, ModelParams& p);

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
     * Destroys the pattern instance constraints contained by this object.
     */
    void
    destroyConstraintsForPIs(void);

  protected:

    /**
     * Numbers of action nodes in the function.
     */
    size_t num_func_action_nodes_;

    /**
     * Numbers of data nodes in the function.
     */
    size_t num_func_data_nodes_;

    /**
     * Numbers of state nodes in the function.
     */
    size_t num_func_state_nodes_;

    /**
     * Numbers of label nodes in the function.
     */
    size_t num_func_label_nodes_;

    /**
     * Numbers of registers in the target machine.
     */
    size_t num_regs_;

    /**
     * Numbers of pattern instances.
     */
    size_t num_pis_;

    /**
     * The dominator sets for each label node in the function graph.
     */
    std::vector< std::list<ArrayIndex> > func_label_domsets_;

    /**
     * The root label which indicates the entry point in the function graph.
     */
    ArrayIndex func_root_label_;

    /**
     * The constraints for the function graph. The constraints are destroyed
     * when this object is deleted.
     */
    std::list<const Constraint*> func_constraints_;

    /**
     * The code size (in bytes) of the instruction for each pattern instance.
     */
    std::vector<int> pat_inst_code_sizes_;

    /**
     * The latency (in cycles) of the instruction for each pattern instance.
     */
    std::vector<int> pat_inst_latencies_;

    /**
     * The action nodes in the function graph which are covered by each pattern
     * instance.
     */
    std::vector< std::list<ArrayIndex> > pat_inst_actions_covered_;

    /**
     * The data nodes in the function graph which are defined by each pattern
     * instance.
     */
    std::vector< std::list<ArrayIndex> > pat_inst_data_defined_;

    /**
     * The data nodes in the function graph which are used by each pattern
     * instance.
     */
    std::vector< std::list<ArrayIndex> > pat_inst_data_used_;

    /**
     * The state nodes in the function graph which are defined by each pattern
     * instance.
     */
    std::vector< std::list<ArrayIndex> > pat_inst_states_defined_;

    /**
     * The state nodes in the function graph which are used by each pattern
     * instance.
     */
    std::vector< std::list<ArrayIndex> > pat_inst_states_used_;

    /**
     * The label nodes in the function graph which are referred to by each
     * pattern instance.
     */
    std::vector< std::list<ArrayIndex> > pat_inst_labels_referred_;

    /**
     * The constraints for each pattern instance. The constraints are destroyed
     * when this object is deleted.
     */
    std::vector< std::list<const Constraint*> > pat_inst_constraints_;

    /**
     * Whether use-def-dom constraints should be applied on a particular pattern
     * instance. The ability to turn these off are required by the generic phi
     * patterns.
     */
    std::vector<bool> pat_inst_use_def_dom_constraints_;
};

}

#endif
