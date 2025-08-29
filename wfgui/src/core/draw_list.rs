use alloc::vec::Vec;
use core::alloc::Allocator;

use wfcommon::cast_u32;
use wfmath::Box2;
use wfmath::Vec2;
use wfmath::Vec4;
use wfmath::vec2;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
#[derive(bytemuck::NoUninit)]
pub struct Command {
    pub scissor_rect: Box2,
    pub texture_id: u64,
    pub index_count: u32,
    pub _pad: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
#[derive(bytemuck::NoUninit)]
pub struct Vertex {
    pub position: Vec2,
    pub tex_coord: Vec2,
    pub color: Vec4,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DrawList<A: Allocator + Clone> {
    commands: Vec<Command, A>,
    vertices: Vec<Vertex, A>,
    indices: Vec<u32, A>,
}

impl<A: Allocator + Clone> DrawList<A> {
    pub fn with_capacity_in(capacity: usize, allocator: A) -> Self {
        let vertex_capacity: usize = capacity * 4;
        let index_capacity: usize = capacity * 6;

        Self {
            commands: Vec::with_capacity_in(capacity, allocator.clone()),
            vertices: Vec::with_capacity_in(vertex_capacity, allocator.clone()),
            indices: Vec::with_capacity_in(index_capacity, allocator),
        }
    }

    pub fn commands(&self) -> &[Command] {
        &self.commands
    }

    pub fn vertices(&self) -> &[Vertex] {
        &self.vertices
    }

    pub fn indices(&self) -> &[u32] {
        &self.indices
    }

    pub fn draw_rect(&mut self, rect: Box2, texture_rect: Box2, color: Vec4, scissor_rect: Box2, texture_id: u64) {
        let tl_position = vec2(rect.x, rect.y);
        let tl_tex_coord = vec2(texture_rect.x, texture_rect.y);

        let tr_position = vec2(rect.max_x(), rect.y);
        let tr_tex_coord = vec2(texture_rect.max_x(), texture_rect.y);

        let bl_position = vec2(rect.x, rect.max_y());
        let bl_tex_coord = vec2(texture_rect.x, texture_rect.max_y());

        let br_position = vec2(rect.max_x(), rect.max_y());
        let br_tex_coord = vec2(texture_rect.max_x(), texture_rect.max_y());

        let index_base = cast_u32(self.vertices.len());

        self.vertices.push(Vertex {
            position: bl_position,
            tex_coord: bl_tex_coord,
            color,
        });
        self.vertices.push(Vertex {
            position: br_position,
            tex_coord: br_tex_coord,
            color,
        });
        self.vertices.push(Vertex {
            position: tr_position,
            tex_coord: tr_tex_coord,
            color,
        });
        self.vertices.push(Vertex {
            position: tl_position,
            tex_coord: tl_tex_coord,
            color,
        });

        // 0, 1, 2
        let i1 = index_base;
        let i2 = index_base + 1;
        let i3 = index_base + 2;
        // 2, 3, 0
        let i4 = index_base + 2;
        let i5 = index_base + 3;
        let i6 = index_base;

        self.indices.push(i1);
        self.indices.push(i2);
        self.indices.push(i3);
        self.indices.push(i4);
        self.indices.push(i5);
        self.indices.push(i6);

        if let Some(ref mut last_command) = self.commands.last_mut() {
            if last_command.scissor_rect == scissor_rect && last_command.texture_id == texture_id {
                last_command.index_count += 6;
            } else {
                self.commands.push(Command {
                    scissor_rect,
                    texture_id,
                    index_count: 6,
                    _pad: 0,
                });
            }
        } else {
            self.commands.push(Command {
                scissor_rect,
                texture_id,
                index_count: 6,
                _pad: 0,
            });
        }
    }

    pub fn clear(&mut self) {
        self.commands.clear();
        self.vertices.clear();
        self.indices.clear();
    }
}
