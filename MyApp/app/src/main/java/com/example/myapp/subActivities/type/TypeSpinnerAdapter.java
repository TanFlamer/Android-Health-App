package com.example.myapp.subActivities.type;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.databaseFiles.type.Type;

import java.util.List;

public class TypeSpinnerAdapter extends ArrayAdapter<Type> {

    private List<Type> typeList;

    public TypeSpinnerAdapter(@NonNull Context context, int resource, List<Type> typeList) {
        super(context, resource, typeList);
        this.typeList = typeList;
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        TextView label = (TextView) super.getView(position, convertView, parent);
        label.setText(typeList.get(position).getTypeName());
        return label;
    }

    @Override
    public View getDropDownView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        TextView label = (TextView) super.getView(position, convertView, parent);
        label.setText(typeList.get(position).getTypeName());
        return label;
    }
}
