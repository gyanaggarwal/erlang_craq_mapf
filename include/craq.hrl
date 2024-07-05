-define(CRAQ_NODE_ORDER_SORTED,                            craq_sorted).
-define(CRAQ_NODE_ORDER_USER_DEFINED,                      craq_user_defined).

-define(CRAQ_UNDEFINED,                                    undefined).

-define(CRAQ_PRED_PRE_UPDATE,                              craq_pred_pre_update).
-define(CRAQ_SUCC_UPDATE,                                  craq_succ_update).
-define(CRAQ_UPDATE,                                       craq_update).
-define(CRAQ_ADD_NODE,                                     craq_add_node).
-define(CRAQ_DROP_NODE,                                    craq_drop_node).
-define(CRAQ_FORWARD_MSG,                                  craq_forward_msg).
-define(CRAQ_NODE_OUTPUT_MSG,                              craq_node_output_msg).
-define(CRAQ_MSG_REF_LIST_MSG,                             craq_msg_ref_list_msg).
-define(CRAQ_REQ_SNAPSHOT,                                 craq_req_snapshot).
-define(CRAQ_PROCESS_SNAPSHOT,                             craq_process_snapshot).
-define(CRAQ_RES_SNAPSHOT,                                 craq_res_snapshot).
-define(CRAQ_REQ_NODE_STATE,                               craq_req_node_state).
-define(CRAQ_SET_REPL_RING,                                craq_set_repl_ring).
-define(CRAQ_REPL_RING,                                    craq_repl_ring).
-define(CRAQ_PRECEDENCE_ORDER_RING,                        craq_precedence_order_ring).

-define(CRAQ_NODE_READY,                                   craq_node_ready).
-define(CRAQ_NODE_NOT_READY,                               craq_node_not_ready).
-define(CRAQ_NODE_TRANSIENT,                               craq_node_trasient).

-define(CRAQ_INVALID_MSG,                                  craq_invalid_msg).
-define(CRAQ_VALID_NEW_NODE_MSG,                           craq_valid_new_node_msg).
-define(CRAQ_VALID_EXISTING_NODE_MSG,                      craq_valid_existing_node_msg).
-define(CRAQ_RING_MSG,                                     craq_ring_msg).
-define(CRAQ_HEAD_MSG,                                     craq_head_msg).
-define(CRAQ_TAIL_MSG,                                     craq_tail_msg).

-define(CRAQ_UPDATED,                                      craq_updated).
-define(CRAQ_BEING_UPDATED,                                craq_being_updated).
-define(CRAQ_BEING_UPDATED_HEAD,                           craq_being_updated_head).
-define(CRAQ_BEING_UPDATED_CAUSALLY,                       craq_being_updated_causally).
-define(CRAQ_NODE_UNAVAILABLE,                             craq_node_unavailable).
-define(CRAQ_UPDATE1,                                      craq_update1).
-define(CRAQ_PRED_PRE_UPDATE1,                             craq_pred_pre_update1).
-define(CRAQ_PRE_UPDATE_DOWN,                              craq_pre_update_down).
-define(CRAQ_UPDATE_DOWN,                                  craq_update_down).

-define(CRAQ_PROCESS_NODEDOWN,                             craq_process_nodedown).

-define(CRAQ_CRITERIA_VALUE,                               craq_criteria_value).

-define(CRAQ_DEFAULT_PRECEDENCE_ORDER,                     0).

-record(craq_next_step,                                   {next_tag                                 :: term(),
							   update_logical_clock=true                :: true | false,
							   update_data_view=false                   :: true | false,
							   being_updated_msg_list=[]                :: list(),
							   make_output=false                        :: true | false,
							   send_to_succ=false                       :: true | false,
							   send_to_pred=false                       :: true | false,
							   send_output=false                        :: true | false,
							   add_to_pre_update=false                  :: true | false,
							   add_to_update=false                      :: true | false,
							   move_to_update=false                     :: true | false,
							   move_to_complete=false                   :: true | false,
							   remove_complete=false                    :: true | false,
							   remove_from_complete=false               :: true | false}).

-record(craq_server_msg,                                  {mref_map=maps:new()                      :: maps:map()}).

-record(craq_server_state,                                {repl_ring=[]                             :: list(),
							   precedence_order_ring=[]                 :: list(),
							   server_msg=#craq_server_msg{}            :: #craq_server_msg{},
							   config_map=maps:new()                    :: maps:map()}).

-record(craq_node_state,                                  {node_id                                  :: atom(),
							   node_process                             :: term(),
							   node_state=?CRAQ_NODE_NOT_READY          :: ?CRAQ_NODE_NOT_READY |
												       ?CRAQ_NODE_TRANSIENT |
                                                                                                       ?CRAQ_NODE_READY,
                                                           logical_clock=0                          :: non_neg_integer(),
							   precedence_order                         :: term(),
							   repl_ring=[]                             :: list(),
							   predecessor                              :: atom(),
							   successor                                :: atom(),
							   pre_update_msg_list=[]                   :: list(),
							   update_msg_list=[]                       :: list(),
							   completed_msg_list=[]                    :: list(),
							   snapshot_ref                             :: term(),
							   data_view                                :: term(),
							   config_map=maps:new()                    :: maps:map()}).

-record(craq_update_msg,                                  {logical_clock=0                          :: non_neg_integer(),
                                                           msg_node_id                              :: term(),
							   server_msg_ref                           :: term(),
                                                           client_msg_ref                           :: term(),
                                                           client_id                                :: term(),
                                                           precedence_order                         :: term(),
                                                           input_msg                                :: term(),
							   output_msg=false                         :: false | {true, term()},
							   completed_msg_list                       :: list(),
                                                           update_msg_list                          :: list()}).



                                                        
                                                           