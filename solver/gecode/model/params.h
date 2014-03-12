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
 *
 * The views and conclusions contained in the software and documentation are
 * those of the authors and should not be interpreted as representing official
 * policies, either expressed or implied, of the FreeBSD Project.
 */

#ifndef SOLVER_GECODE_MODEL_MODEL__
#define SOLVER_GECODE_MODEL_MODEL__

#include <map>
#include <string>
#include <vector>

namespace Model {

/**
 * @todo write description
 */
class Params {
  public:
    /**
     * Parses a JSON string into an internal model parameters object.
     *
     * @param str
     *        String containing the JSON data.
     * @param params
     *        The Params object to write to.
     * @throws exception
     *         When parsing fails.
     */
    static void
    parseJson(const std::string& str, Params& params);

  protected:
    /**
     * The number of action nodes in the function.
     */
    size_t func_num_action_nodes_;

    /**
     * Same as #func_num_action_nodes_ but for data nodes.
     */
    size_t func_num_data_nodes_;

    /**
     * Same as #func_num_action_nodes_ but for label nodes.
     */
    size_t func_num_label_nodes_;

    /**
     * Same as #func_num_action_nodes_ but for state nodes.
     */
    size_t func_num_state_nodes_;

    /**
     * Maps the node ID of an action node to an array index.
     */
    std::map<size_t, size_t> func_action_node_mappings_;

    /**
     * Same as #func_action_node_mappings_ but for data nodes.
     */
    std::map<size_t, size_t> func_data_node_mappings_;

    /**
     * Same as #func_action_node_mappings_ but for label nodes.
     */
    std::map<size_t, size_t> func_label_node_mappings_;

    /**
     * Same as #func_action_node_mappings_ but for state nodes.
     */
    std::map<size_t, size_t> func_state_node_mappings_;

    /**
     * The dominator sets for each label node in the function.
     */
    std::vector< std::vector<size_t> > func_label_domsets_;

    /**
     * The number of pattern instances.
     */
    size_t pat_num_instances_;
};

}

#endif
