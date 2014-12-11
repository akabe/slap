open Slap.Size
open Slap.D

type actv =
  | Sigmoid
  | Tanh

type ('indim, 'loutdim, 'outdim, 'upper) t
type 'acc fld =
  {
    fld : 'indim 'loutdim 'outdim 'upper.
          ('indim, 'loutdim, 'outdim, 'upper) t ->
          'acc -> 'acc;
  }

(** [create ?dropout actv m n] creates a single-layer neural network of
    [m] outputs and [n] inputs. [actv] is an activation function.

    @param dropout a dropout probability for inputs (default = no dropout)
 *)
val create : ?dropout:float -> actv ->
             'outdim Slap.Size.t -> 'indim Slap.Size.t ->
             ('indim, 'outdim, 'outdim, unit) t

(** [append_layer ?dropout actv n nnet] appends a layer with [n] inputs to
    a neural network [nnet]. [actv] is an activation function.

    @param dropout a dropout probability for inputs (default = no dropout)
 *)
val append_layer : ?dropout:float -> actv -> 'indim Slap.Size.t ->
                   ('loutdim, 'uploutdim, 'outdim, 'upup) t ->
                   ('indim, 'loutdim, 'outdim,
                    ('loutdim, 'uploutdim, 'outdim, 'upup) t) t

(** [fold nnet fld init] folds each layer in [nnet] bottom to top. *)
val fold : ('indim, 'loutdim, 'outdim, 'upper) t ->
           'acc fld -> 'acc -> 'acc

(** [exec nnet x] computes an output vector of [nnet]. [x] is an augumented
    input vector, i.e., the last element in [x] is always [1.0].
 *)
val exec : ('indim, 'loutdim, 'outdim, 'upper) t ->
           ('indim s, Slap.cnt) vec -> ('outdim, Slap.cnt) vec

(** [init_weights ?from ?range nnet] initializes weights in [nnet] randomly. *)
val init_weights : ?from:float -> ?range:float ->
                   ('indim, 'loutdim, 'outdim, 'upper) t -> unit

(** [get_error nnet samples] evaluates the error function of [nnet]. *)
val get_error : ('indim, 'loutdim, 'outdim, 'upper) t ->
                (('indim s, Slap.cnt) vec * ('outdim, Slap.cnt) vec) array ->
                float

(** [online_train ?eta ?check nnet x t] updates weights in [nnet] by online
    backpropagation.

    @param eta a learning rate (default = [0.5])
    @param check [true] if gradient is checked (default = [true])
 *)
val online_train : ?eta:float -> ?check:bool ->
                   ('indim, 'loutdim, 'outdim, 'upper) t ->
                   ('indim s, Slap.cnt) vec -> ('outdim, Slap.cnt) vec ->
                   unit

(** [batch_train ?eta ?check nnet samples] updates weights in [nnet] by batch
    backpropagation.

    @param eta a learning rate (default = [0.5])
    @param check [true] if gradient is checked (default = [true])
 *)
val batch_train : ?eta:float -> ?check:bool ->
                  ('indim, 'loutdim, 'outdim, 'upper) t ->
                  (('indim s, Slap.cnt) vec * ('outdim, Slap.cnt) vec) array ->
                  unit

(** [minibatch_train ?eta ?check nnet n samples] updates weights in [nnet] by
    mini-batch backpropagation. [n] is a mini-batch size.

    @param eta a learning rate (default = [0.5])
    @param check [true] if gradient is checked (default = [true])
 *)
val minibatch_train : ?eta:float -> ?check:bool ->
                      ('indim, 'loutdim, 'outdim, 'upper) t -> int ->
                      (('indim s, Slap.cnt) vec * ('outdim, Slap.cnt) vec) array ->
                      unit
