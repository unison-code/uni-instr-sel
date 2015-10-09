/**
 * Copyright (c) 2013-2015, Gabriel Hjort Blindell <ghb@kth.se>
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
     * Gets the number of operations in the function graph.
     *
     * @returns Number of operations.
     */
    size_t
    getNumOperationsInF(void) const;

    /**
     * Gets the number of data in the function graph.
     *
     * @returns Number of data.
     */
    size_t
    getNumDataInF(void) const;

    /**
     * Gets the number of blocks in the function graph.
     *
     * @returns Number of blocks.
     */
    size_t
    getNumBlocksInF(void) const;

    /**
     * Gets the number of locations in the target machine.
     *
     * @returns Number of locations.
     */
    size_t
    getNumLocationsInM(void) const;

    /**
     * Gets the number of matches.
     *
     * @returns Number of matches.
     */
    size_t
    getNumMatches(void) const;

    /**
     * Gets the dominator set per given block in the function graph.
     *
     * @returns List of blocks that dominates a particular block.
     */
    std::vector< std::list<ArrayIndex> >
    getBlockDomSetsInF(void) const;

    /**
     * Gets the list of definition edges per given block in the function graph.
     *
     * @returns List of data between which there is a definition edge with a
     * particular block.
     */
    std::vector< std::list<ArrayIndex> >
    getDefEdgesInF(void) const;

    /**
     * Gets the entry block in the function graph.
     *
     * @returns Array index for a block.
     */
    ArrayIndex
    getEntryBlockInF(void) const;

    /**
     * Gets the data in the function that are states.
     *
     * @returns List of datum array indices.
     */
    std::list<ArrayIndex>
    getAllStateDataInF(void) const;

    /**
     * Gets execution frequencies per block the function graph.
     *
     * @returns The execution frequencies.
     */
    std::vector<int>
    getExecFreqOfAllBlocksInF(void) const;

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
     * Gets the matches that have been derived from non-copy instructions.
     *
     * @returns List of match array indices.
     */
    std::list<ArrayIndex>
    getNonCopyInstrMatches(void) const;

    /**
     * Gets the matches that have been derived from null-copy instructions.
     *
     * @returns List of match array indices.
     */
    std::list<ArrayIndex>
    getNullCopyInstrMatches(void) const;

    /**
     * Gets the function operations covered per matches.
     *
     * @returns Vector of list of operation array indices.
     */
    std::vector< std::list<ArrayIndex> >
    getOperationsCoveredByAllMatches(void) const;

    /**
     * Gets the function data defined per match.
     *
     * @returns Vector of list of datum array indices.
     */
    std::vector< std::list<ArrayIndex> >
    getDataDefinedByAllMatches(void) const;

    /**
     * Gets the function data used per match.
     *
     * @returns Vector of list of datum array indices.
     */
    std::vector< std::list<ArrayIndex> >
    getDataUsedByAllMatches(void) const;

    /**
     * Gets the function blocks per match that are entries.
     *
     * @returns Vector of list of block array indices.
     */
    std::vector< std::list<ArrayIndex> >
    getEntryBlockOfAllMatches(void) const;

    /**
     * Gets the function blocks that are spanned by the respective match.
     *
     * @returns Vector of list of block array indices.
     */
    std::vector< std::list<ArrayIndex> >
    getSpannedBlocksInAllMatches(void) const;

    /**
     * Gets the function blocks that are consumed by the respective match.
     *
     * @returns Vector of list of block array indices.
     */
    std::vector< std::list<ArrayIndex> >
    getConsumedBlocksInAllMatches(void) const;

    /**
     * Gets the apply-def-dom-use-constraint settings per match.
     *
     * @returns Vector of booleans.
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
     * Sets the entry block for the function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setEntryBlockInF(const Json::Value& root, ModelParams& p);

    /**
     * Sets the dominator sets for the blocks in the function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setBlockDomSetsInF(const Json::Value& root, ModelParams& p);

    /**
     * Sets the definition edges for the respective blocks in the function
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
    setDefEdgesInF(const Json::Value& root, ModelParams& p);

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
     * Sets the execution frequencies of the blocks in the function graph.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setExecFreqOfBlocksInF(const Json::Value& root, ModelParams& p);

    /**
     * Sets the data in the function that are states.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setStateDataInF(const Json::Value& root, ModelParams& p);

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
     * Sets the matches that have been derived from non-copy instructions.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setNonCopyInstrMatches(const Json::Value& root, ModelParams& p);

    /**
     * Sets the matches that have been derived from null-copy instructions.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setNullCopyInstrMatches(const Json::Value& root, ModelParams& p);

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
     * Sets the function operations covered by the respective match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setOperationsCoveredByMatches(const Json::Value& root, ModelParams& p);

    /**
     * Sets the function data defined by the respective match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setDataDefinedByMatches(const Json::Value& root, ModelParams& p);

    /**
     * Sets the function data used by the respective match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setDataUsedByMatches(const Json::Value& root, ModelParams& p);

    /**
     * Sets the function blocks that is the entry block of each respective
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
    setEntryBlockOfMatches(const Json::Value& root, ModelParams& p);

    /**
     * Sets the function blocks that are spanned by the respective match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setSpannedBlocksInMatches(const Json::Value& root, ModelParams& p);

    /**
     * Sets the function blocks that are consumed by the respective match.
     *
     * @param root
     *        The JSON root value.
     * @param p
     *        Object to add the data to.
     * @throws Exception
     *         When an error occurs.
     */
    static void
    setConsumedBlocksInMatches(const Json::Value& root, ModelParams& p);

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
     * Numbers of operations in the function.
     */
    size_t num_func_operations_;

    /**
     * Numbers of data in the function.
     */
    size_t num_func_data_;

    /**
     * Numbers of blocks in the function.
     */
    size_t num_func_blocks_;

    /**
     * Numbers of locations in the target machine.
     */
    size_t num_locations_;

    /**
     * Numbers of matches.
     */
    size_t num_matches_;

    /**
     * The block which indicates the entry point in the function graph.
     */
    ArrayIndex func_entry_block_;

    /**
     * The dominator sets for each block in the function graph.
     */
    std::vector< std::list<ArrayIndex> > func_block_dom_sets_;

    /**
     * The data between which there is a definition edge with the respective
     * block.
     */
    std::vector< std::list<ArrayIndex> > func_def_edges_;

    /**
     * The data in the function graph that are states.
     */
    std::list<ArrayIndex> func_states_;

    /**
     * The execution frequency per block (as identified by the blocks) in the
     * function graph.
     */
    std::vector<int> func_block_exec_freq_;

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
     * The operations in the function graph which are covered by each match.
     */
    std::vector< std::list<ArrayIndex> > match_operations_covered_;

    /**
     * The data in the function graph which are defined by each match.
     */
    std::vector< std::list<ArrayIndex> > match_data_defined_;

    /**
     * The data in the function graph which are used by each match.
     */
    std::vector< std::list<ArrayIndex> > match_data_used_;

    /**
     * The entry block, if any, for each match.
     */
    std::vector< std::list<ArrayIndex> > match_entry_block_;

    /**
     * The blocks in the function graph spanned by each match.
     */
    std::vector< std::list<ArrayIndex> > match_spanned_blocks_;

    /**
     * The blocks in the function graph consumed by each match.
     */
    std::vector< std::list<ArrayIndex> > match_consumed_blocks_;

    /**
     * The matches that have been derived from non-copy instructions.
     */
    std::list<ArrayIndex> match_non_copy_instrs_;

    /**
     * The matches that have been derived from null-copy instructions.
     */
    std::list<ArrayIndex> match_null_copy_instrs_;

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
